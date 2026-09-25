#' Download the Global Macro Database and the underlying raw data, with version control
#'
#' This function downloads and loads the Global Macro Database (GMD), the world's
#' most comprehensive repository of macroeconomic statistics. Users can specify which
#' version to load, which variables to keep, and filter for specific countries. Users
#' can also download the underlying data, which means easy access to hundreds of
#' cleaned data sources from the original providers. The dataset is updated quarterly,
#' with occasional patches; versions follow the naming convention YYYY_MM.
#'
#' When variables are specified, the function automatically drops observations where
#' all specified variables are missing.
#'
#' @param variables A character vector of variable names to include (e.g., \code{"rGDP"}
#'   or \code{c("rGDP", "unemp")}). Case-insensitive (e.g. \code{"rgdp"} matches
#'   \code{"rGDP"}). Can also be used with \code{sources} to load specific variables
#'   from a given source.
#' @param country A character vector of ISO3 country codes (e.g., \code{"USA"} or
#'   \code{c("USA", "CHN")}). Case-insensitive.
#' @param version A string specifying which version of the dataset to load (e.g.,
#'   \code{"2025_01"}). Use \code{"current"} to display the current version, or
#'   \code{"list"} to see all available versions.
#' @param raw A logical. If \code{TRUE}, load all raw data sources for a single
#'   specified variable. Requires specifying exactly one variable.
#' @param iso A logical. If \code{TRUE}, display list of available countries with
#'   ISO3 codes.
#' @param vars A logical. If \code{TRUE}, display list of available variables with
#'   definitions and units.
#' @param sources A string. \code{"load"} to load the source list as a dataframe,
#'   \code{"list"} to print available source names, or a specific source name
#'   (e.g., \code{"IMF_IFS"}) to load that source's cleaned data. You can combine
#'   with \code{variables} to load only specific variables from that source.
#' @param cite A string. \code{"load"} to load the full citation list as a dataframe,
#'   or a specific source key (e.g., \code{"GMD"}) to display its BibTeX citation.
#' @param print_option A string, \code{"GMD"} or \code{"Stata"} (case-insensitive),
#'   to print the corresponding APA citation and return invisibly. Parity with the
#'   Python/Stata \code{print} option.
#' @param fast Logical (or the string \code{"yes"}). If \code{TRUE}, save the
#'   downloaded dataset to a local cache so subsequent calls load it from disk
#'   instead of re-downloading. Once cached, the file is reused automatically.
#'   Parity with the Python/Stata \code{fast} option.
#' @param start_year A single number. If supplied, only observations with
#'   \code{year >= start_year} are returned.
#' @param end_year A single number. If supplied, only observations with
#'   \code{year <= end_year} are returned.
#' @return A dataframe containing the requested macroeconomic data.
#'
#' @examples
#' \dontrun{
#' # Load the latest full dataset
#' df <- gmd()
#'
#' # Load specific variables
#' df <- gmd(variables = c("rGDP", "infl", "unemp"))
#'
#' # Load data for a specific country
#' df <- gmd(country = "USA")
#'
#' # Restrict to a range of years
#' df <- gmd(country = "USA", variables = "rGDP", start_year = 2000, end_year = 2010)
#'
#' # Load a specific version for reproducibility
#' df <- gmd(version = "2025_01")
#'
#' # List all available versions
#' gmd(version = "list")
#'
#' # Access raw data for a single variable
#' df <- gmd(variables = "rGDP", raw = TRUE)
#'
#' # List available variables
#' df <- gmd(vars = TRUE)
#'
#' # List available countries
#' df <- gmd(iso = TRUE)
#'
#' # Access data from a specific source
#' df <- gmd(sources = "IMF_WEO")
#'
#' # Access specific variables from a source
#' df <- gmd(sources = "IMF_WEO", variables = "nGDP")
#'
#' # List all available sources
#' gmd(sources = "list")
#'
#' # Get BibTeX citation
#' gmd(cite = "GMD")
#' }
#'
#' @section Citation:
#' When using the Global Macro Database, please cite:
#'
#' Müller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database:
#' A New International Macroeconomic Dataset (NBER Working Paper No. 33714).
#'
#' Use \code{gmd(cite = "GMD")} to get the BibTeX code.
#'
#' @section Documentation:
#' \itemize{
#'   \item \href{https://www.globalmacrodata.com}{Website}
#'   \item \href{https://gmd-releases.s3.ap-southeast-2.amazonaws.com/data/distribute/GMD_TA.pdf}{Technical Appendix}
#'   \item \href{https://github.com/KMueller-Lab/Global-Macro-Database}{Project source code}
#' }
#'
#' @author
#' Mohamed Lehbib (\email{lehbib@@u.nus.edu}), National University of Singapore
#'
#' Karsten Müller (\email{kmueller@@nus.edu.sg}), National University of Singapore
#'
#' @export
gmd <- function(variables = NULL, country = NULL, version = NULL,
                raw = FALSE, iso = FALSE, vars = FALSE,
                sources = NULL, cite = NULL, print_option = NULL,
                fast = FALSE, start_year = NULL, end_year = NULL) {

  base_url <- "https://gmd-releases.s3.ap-southeast-2.amazonaws.com/data"
  ID_COLS <- c("ISO3", "year", "countryname", "id")

  # [print] Print the APA citation and return early (parity with Python/Stata).
  # Case-insensitive; invalid value errors.
  if (!is.null(print_option)) {
    if (length(print_option) != 1 || is.na(print_option)) {
      stop("`print_option` must be a single non-NA value ('GMD' or 'Stata').")
    }
    opt <- tolower(trimws(print_option))
    if (opt == "gmd") {
      message("M\u00fcller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database: A New International Macroeconomic Dataset (NBER Working Paper No. 33714).")
      return(invisible(NULL))
    }
    if (opt == "stata") {
      message("Lehbib, M. & M\u00fcller, K. (2025). gmd: The Easy Way to Access the World's Most Comprehensive Macroeconomic Database. Working Paper.")
      return(invisible(NULL))
    }
    stop("Invalid option for print(). valid arguments are 'GMD' or 'Stata'.")
  }

  message("Global Macro Database by M\u00fcller, Xu, Lehbib, and Chen (2025)")
  message("Website: https://www.globalmacrodata.com")
  message("")

  # --- Input validation & normalization (runs before any download) ---

  # Trim incidental whitespace on version/country/variables. An empty result
  # after trimming still hits the errors below (rather than silently becoming
  # "no filter"), consistent with #318's fix for empty vectors triggering a
  # full dataset download before failing.
  if (!is.null(version) && is.character(version) && length(version) == 1 && !is.na(version)) {
    version <- trimws(version)
  }
  if (!is.null(version) &&
      (!is.character(version) || length(version) != 1 || is.na(version) || version == "")) {
    stop("`version` must be a single non-NA character string, or NULL.")
  }

  if (!is.null(country)) {
    if (!is.character(country)) {
      stop("`country` must be a character vector of ISO3 codes, or NULL.")
    }
    country <- trimws(country)
    country <- country[!is.na(country) & country != ""]
    if (length(country) == 0) {
      stop("`country` must contain at least one ISO3 code, or be NULL.")
    }
  }
  if (!is.null(variables)) {
    if (!is.character(variables)) {
      stop("`variables` must be a character vector of variable names, or NULL.")
    }
    variables <- trimws(variables)
    variables <- variables[!is.na(variables) & variables != ""]
    if (length(variables) == 0) {
      stop("`variables` must contain at least one variable name, or be NULL.")
    }
  }

  # Validate the logical flags `iso`/`vars`: accept only TRUE/FALSE (logical) or
  # the strings "TRUE"/"FALSE" (case-insensitive). Anything else (e.g. "yes", 1,
  # NA) is a clear error rather than a cryptic crash in the `&&` / `if` checks.
  as_flag <- function(x, name) {
    if (is.logical(x) && length(x) == 1L && !is.na(x)) return(x)
    if (is.character(x) && length(x) == 1L && !is.na(x) && toupper(x) %in% c("TRUE", "FALSE")) {
      return(toupper(x) == "TRUE")
    }
    stop(sprintf("`%s` must be TRUE or FALSE (or the string \"TRUE\"/\"FALSE\"). You supplied: %s",
                 name, paste(deparse(x), collapse = "")))
  }
  iso  <- as_flag(iso, "iso")
  vars <- as_flag(vars, "vars")

  validate_year <- function(value, name) {
    if (is.null(value)) return(invisible(NULL))
    if (length(value) != 1 || is.na(value) || !is.numeric(value)) {
      stop(sprintf("`%s` must be a single non-NA number, or NULL.", name))
    }
  }
  validate_year(start_year, "start_year")
  validate_year(end_year, "end_year")
  if (!is.null(start_year) && !is.null(end_year) && start_year > end_year) {
    stop("`start_year` must not be greater than `end_year`.")
  }

  # --- Internal helpers ---

  require_haven <- function() {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' is required for reading .dta files. Please install it using install.packages('haven')")
    }
  }

  load_country_mapping <- function() {
    resp <- .gmd_safe_get(paste0(base_url, "/helpers/countrylist.dta"))
    if (!is.null(resp)) {
      require_haven()
      return(haven::read_dta(httr2::resp_body_raw(resp)))
    }
    # Fallback to bundled CSV
    path <- system.file("isomapping.csv", package = "globalmacrodata")
    if (nzchar(path) && file.exists(path)) {
      message("Loading country list from local fallback.")
      return(readr::read_csv(path,
        col_types = readr::cols(countryname = readr::col_character(),
                                ISO3 = readr::col_character())))
    }
    stop("Unable to load country list. Check internet connection or reinstall the package.")
  }

  load_varlist <- function() {
    resp <- .gmd_safe_get(paste0(base_url, "/helpers/varlist.csv"))
    if (!is.null(resp)) {
      return(readr::read_csv(httr2::resp_body_string(resp, encoding = "UTF-8"),
        col_types = readr::cols(variables = readr::col_character(),
                                units = readr::col_character(),
                                definition = readr::col_character())))
    }
    # Fallback to bundled CSV
    path <- system.file("varlist.csv", package = "globalmacrodata")
    if (nzchar(path) && file.exists(path)) {
      message("Loading variable list from local fallback.")
      return(readr::read_csv(path,
        col_types = readr::cols(variables = readr::col_character(),
                                units = readr::col_character(),
                                definition = readr::col_character())))
    }
    stop("Unable to load variable list. Check internet connection or reinstall the package.")
  }

  validate_country <- function(country, country_mapping) {
    country <- toupper(country)
    invalid <- country[!country %in% country_mapping$ISO3]
    if (length(invalid) > 0) {
      stop(sprintf("Invalid country code(s): %s\n\nTo see the list of valid country codes, use: gmd(iso = TRUE)",
                  paste(invalid, collapse = ", ")))
    }
    country
  }

  drop_na_cols <- function(df, protect = NULL) {
    all_na <- colnames(df)[vapply(df, function(col) all(is.na(col)), logical(1))]
    if (!is.null(protect)) all_na <- setdiff(all_na, protect)
    if (length(all_na) > 0) df[, !colnames(df) %in% all_na, drop = FALSE] else df
  }

  reorder_cols <- function(df, first = c("ISO3", "countryname", "year")) {
    first <- intersect(first, colnames(df))
    rest <- setdiff(colnames(df), first)
    df[, c(first, rest), drop = FALSE]
  }

  apply_year_filter <- function(df) {
    if (!is.null(start_year)) df <- df[df$year >= start_year, , drop = FALSE]
    if (!is.null(end_year))   df <- df[df$year <= end_year, , drop = FALSE]
    df
  }

  print_citation <- function(version) {
    message(sprintf("Version: %s", version))
    message("")
    message("When using these data, please cite:")
    message("M\u00fcller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database.")
    message("To get BibTeX: gmd(cite = 'GMD')")
  }

  # --- Lazy caches (fetched only when first needed) ---
  country_mapping_cache <- NULL
  get_country_mapping <- function() {
    if (is.null(country_mapping_cache)) {
      country_mapping_cache <<- load_country_mapping()
    }
    country_mapping_cache
  }

  varlist_cache <- NULL
  get_varlist <- function() {
    if (is.null(varlist_cache)) {
      varlist_cache <<- load_varlist()
    }
    varlist_cache
  }

  # ============================================================================
  # Mutual-exclusion warnings
  # ============================================================================
  if (iso && vars) {
    warning("You can only show either countries or variables, not both!")
    return(invisible(NULL))
  }

  if ((iso || vars) &&
      (!is.null(variables) || !is.null(country) || !is.null(version) ||
       raw != FALSE || !is.null(sources) || !is.null(cite))) {
    stop("When iso = TRUE or vars = TRUE, do not supply other inputs (variables, country, version, raw, sources, cite).")
  }

  # ============================================================================
  # ISO listing
  # ============================================================================
  if (iso) {
    cm <- get_country_mapping()
    df <- data.frame(
      "Country_and_territories" = cm$countryname,
      "Code" = cm$ISO3,
      stringsAsFactors = FALSE
    )
    message("Loading countries list")
    return(df)
  }

  # ============================================================================
  # Variable listing
  # ============================================================================
  if (vars) {
    vl <- get_varlist()
    df <- data.frame(
      Variable = vl$variables,
      Definition = vl$definition,
      Units = vl$units,
      stringsAsFactors = FALSE
    )
    class(df) <- c("gmd_vars", class(df))
    message("Loading variables list")
    return(df)
  }

  # ============================================================================
  # Cite option
  # ============================================================================
  if (!is.null(cite)) {
    # [#7] Validate an explicit version before honoring cite, so an invalid
    # version errors regardless of cite (matches Python/Stata precedence).
    if (!is.null(version) && !tolower(version) %in% c("list", "current")) {
      .v_avail <- sort(unique(.gmd_load_versions_df()$versions), decreasing = TRUE)
      if (!version %in% .v_avail) {
        stop(sprintf("Error: %s is not valid\nAvailable versions are: %s\nThe current version is: %s",
                    version, paste(sort(.v_avail), collapse = ", "), .v_avail[1]))
      }
    }
    cite_resp <- .gmd_safe_get(paste0(base_url, "/helpers/bib_dataframe.csv"))
    if (is.null(cite_resp)) {
      stop("Unable to import the list of sources to cite. Check internet connection.")
    }
    cite_df <- readr::read_csv(httr2::resp_body_string(cite_resp, encoding = "UTF-8"),
      col_types = readr::cols(source = readr::col_character(),
                              citation = readr::col_character()))

    if (tolower(cite) == "load") {
      message("Imported the list of sources to cite.")
      return(as.data.frame(cite_df))
    }

    matched <- cite_df[tolower(cite_df$source) == tolower(cite), ]
    if (nrow(matched) == 0) {
      stop(sprintf("Source '%s' does not exist.\nTo load the list of sources to cite, use: gmd(cite = 'load')", cite))
    }
    message(matched$citation[1])
    return(invisible(as.data.frame(matched)))
  }

  # ============================================================================
  # Version check (S3 first, GitHub fallback)
  # ============================================================================
  versions_df <- .gmd_load_versions_df()
  available_versions <- sort(unique(versions_df$versions), decreasing = TRUE)
  current_version <- available_versions[1]

  if (!is.null(version) && tolower(version) == "list") {
    message("Available versions:")
    for (v in sort(available_versions)) message(v)
    return(invisible(available_versions))
  }

  if (!is.null(version)) {
    if (tolower(version) == "current") {
      message(sprintf("Current version: %s", current_version))
      data_url <- paste0(base_url, "/distribute/GMD_", current_version, ".dta")
    } else {
      if (!version %in% available_versions) {
        stop(sprintf("%s is not valid\nAvailable versions are: %s\nThe current version is: %s",
                    version, paste(sort(available_versions), collapse = ", "), current_version))
      }
      data_url <- paste0(base_url, "/distribute/GMD_", version, ".dta")
      current_version <- version
    }
  } else {
    data_url <- paste0(base_url, "/distribute/GMD_", current_version, ".dta")
  }

  # ============================================================================
  # Sources option
  # ============================================================================
  if (!is.null(sources)) {
    if (length(sources) != 1) stop("Please specify exactly one source.")

    if (tolower(sources) %in% c("load", "list")) {
      source_resp <- .gmd_safe_get(paste0(base_url, "/helpers/source_list.csv"))
      if (is.null(source_resp)) {
        stop("Unable to load source list. Check internet connection.")
      }
      source_df <- readr::read_csv(httr2::resp_body_string(source_resp, encoding = "UTF-8"),
        col_types = readr::cols(source_name = readr::col_character()))

      if (tolower(sources) == "load") {
        message("Imported the list of sources.")
        return(as.data.frame(source_df))
      }
      message("Available sources:")
      for (s in source_df$source_name) message(s)
      return(invisible(source_df$source_name))
    }

    sources <- trimws(sources)
    source_resp <- .gmd_safe_get(paste0(base_url, "/clean/combined/", sources, ".dta"), quiet = TRUE)

    # Case-insensitive fallback
    if (is.null(source_resp)) {
      sl_resp <- .gmd_safe_get(paste0(base_url, "/helpers/source_list.csv"))
      if (is.null(sl_resp)) {
        stop("Unable to access source list. Check internet connection.")
      }
      sl_df <- readr::read_csv(httr2::resp_body_string(sl_resp, encoding = "UTF-8"),
        col_types = readr::cols(source_name = readr::col_character()))
      matched_source <- sl_df$source_name[tolower(sl_df$source_name) == tolower(sources)]
      if (length(matched_source) == 1) {
        sources <- matched_source
        source_resp <- .gmd_safe_get(paste0(base_url, "/clean/combined/", sources, ".dta"), quiet = TRUE)
      }
      if (is.null(source_resp)) {
        stop(sprintf("Invalid source name: %s\nTo see the list of sources, use: gmd(sources = 'list')", sources))
      }
    }

    require_haven()
    df <- haven::read_dta(httr2::resp_body_raw(source_resp))

    if (!is.null(variables)) {
      source_vars <- paste0(sources, "_", variables)
      # Match source-variable columns case-insensitively (parity with the main
      # path, which canonicalizes variable casing) and return the canonical
      # column names actually present in the data.
      existing <- colnames(df)[tolower(colnames(df)) %in% tolower(source_vars)]
      if (length(existing) == 0) {
        all_data_cols <- setdiff(colnames(df), ID_COLS)
        stop(sprintf("This source doesn't have data on %s. It has data on: %s",
                    paste(variables, collapse = ", "),
                    paste(gsub(paste0("^", sources, "_"), "", all_data_cols), collapse = ", ")))
      }
      id_cols <- intersect(ID_COLS, colnames(df))
      df <- df[, c(id_cols, existing), drop = FALSE]
    }

    if (!is.null(country)) {
      country <- validate_country(country, get_country_mapping())
      df <- df[df$ISO3 %in% country, , drop = FALSE]
    }

    df <- apply_year_filter(df)
    df <- df[order(df$ISO3, df$year), ]
    df <- drop_na_cols(df)

    if (nrow(df) == 0) stop("No data available for the specified parameters")

    n_vars <- ncol(df) - length(intersect(ID_COLS, colnames(df)))
    message(sprintf("Final dataset: %d observations of %d variables", nrow(df), n_vars))
    print_citation(current_version)
    df <- as.data.frame(df)
    return(df)
  }

  # ============================================================================
  # Validate variables
  # ============================================================================
  if (!is.null(variables)) {
    id_names <- c("iso3", "year", "id", "countryname")
    id_matches <- variables[tolower(variables) %in% id_names]
    if (length(id_matches) > 0) {
      stop(sprintf("%s is an identifying variable in the dataset, specify common variables.\nTo see the list of variables, use: gmd(vars = TRUE)",
                  paste(id_matches, collapse = ", ")))
    }

    valid_vars <- get_varlist()$variables
    # Match variable names case-insensitively and normalize to the dataset's
    # canonical casing (e.g. "rgdp" -> "rGDP"), consistent with Python/Stata.
    canonical <- valid_vars[match(tolower(variables), tolower(valid_vars))]
    invalid_vars <- variables[is.na(canonical)]
    if (length(invalid_vars) > 0) {
      stop(sprintf("Invalid variable code(s): %s\n\nTo see the list of valid variable codes, use: gmd(vars = TRUE)",
                  paste(invalid_vars, collapse = ", ")))
    }
    variables <- unique(canonical)
  }

  # ============================================================================
  # Raw data option
  # ============================================================================
  if (raw) {
    if (is.null(variables) || length(variables) != 1) {
      stop("raw requires specifying exactly one variable.\nUsage: gmd(variables = 'var_name', raw = TRUE)")
    }

    message(sprintf("Importing raw data for variable: %s", variables))
    raw_resp <- .gmd_safe_get(paste0(base_url, "/distribute/", variables, "_", current_version, ".csv"))
    if (is.null(raw_resp)) {
      valid_vars <- get_varlist()$variables
      if (variables %in% valid_vars) {
        stop(sprintf("Variable '%s' does not have raw data.", variables))
      } else {
        stop(sprintf("Specified variable '%s' is not valid.\nTo see the list of variables, use: gmd(vars = TRUE)", variables))
      }
    }

    df <- readr::read_csv(httr2::resp_body_string(raw_resp, encoding = "UTF-8"), show_col_types = FALSE)

    if (!is.null(country)) {
      country <- validate_country(country, get_country_mapping())
      df <- df[df$ISO3 %in% country, , drop = FALSE]
    }

    df <- apply_year_filter(df)
    df <- reorder_cols(df)
    df <- df[order(df$countryname, df$year), ]

    if (nrow(df) == 0) stop("No data available for the specified parameters")

    n_sources <- ncol(df) - length(intersect(ID_COLS, colnames(df)))
    message(sprintf("Final dataset: %d observations of %d sources", nrow(df), n_sources))
    print_citation(current_version)
    df <- as.data.frame(df)
    return(df)
  }

  # ============================================================================
  # Main dataset
  # ============================================================================
  require_haven()
  # [fast] Optional local cache of the dataset for faster reloads / offline use
  # (parity with Python/Stata `fast`). Once cached, the file is reused automatically.
  use_fast <- isTRUE(fast) || (is.character(fast) && tolower(trimws(fast)) == "yes")
  cache_dir <- tools::R_user_dir("globalmacrodata", "cache")
  cache_file <- file.path(cache_dir, sprintf("GMD_%s.dta", current_version))
  # Only read the cache when fast = TRUE was explicitly requested; otherwise a
  # cache from a previous fast = TRUE call would silently serve stale data even
  # when the caller asked for a normal (non-cached) load.
  if (use_fast && file.exists(cache_file)) {
    df <- haven::read_dta(cache_file)
  } else {
    main_resp <- .gmd_safe_get(data_url)
    if (is.null(main_resp)) {
      stop(sprintf(paste0("Could not retrieve the data file for version '%s' (%s).\n",
                          "The version is listed as available but the file may be temporarily ",
                          "unavailable on the server; check your internet connection or try another version."),
                  current_version, data_url))
    }
    raw_bytes <- httr2::resp_body_raw(main_resp)
    df <- haven::read_dta(raw_bytes)
    if (use_fast) {
      if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      # Write to a temp file first and rename into place, so a crash or
      # interrupted write never leaves a truncated/corrupt cache file behind.
      tmp_file <- paste0(cache_file, ".tmp", Sys.getpid())
      writeBin(raw_bytes, tmp_file)
      file.rename(tmp_file, cache_file)
      message(sprintf("GMD dataset loaded and saved locally in %s.", cache_dir))
    }
  }

  if (!is.null(country)) {
    country <- validate_country(country, get_country_mapping())
    df <- df[df$ISO3 %in% country, , drop = FALSE]
    message(sprintf("Filtered data for countries: %s", paste(country, collapse = ", ")))
  }

  if (!is.null(variables)) {
    required_cols <- c("ISO3", "countryname", "year", "id")
    available_vars <- intersect(variables, colnames(df))

    if (length(available_vars) == 0) {
      warning("None of the requested variables are available in the dataset.")
    }

    df <- df[, c(required_cols, available_vars), drop = FALSE]

    if (length(available_vars) > 0) {
      has_data <- rowSums(!is.na(df[, available_vars, drop = FALSE])) > 0
      df <- df[has_data, , drop = FALSE]
    }
  }

  df <- apply_year_filter(df)

  df <- drop_na_cols(df, protect = ID_COLS)
  df <- reorder_cols(df)
  df <- df[order(df$countryname, df$year), ]

  n_vars <- ncol(df) - length(intersect(ID_COLS, colnames(df)))
  if (nrow(df) == 0 || n_vars <= 0) {
    stop("No data available for the specified parameters")
  }
  if (n_vars > 1) {
    message(sprintf("Final dataset: %d observations for %d variables", nrow(df), n_vars))
  } else {
    message(sprintf("Final dataset: %d observations for %d variable", nrow(df), n_vars))
  }
  print_citation(current_version)

  df <- as.data.frame(df)
  return(df)
}

#' @export
print.gmd_vars <- function(x, ...) {
  message("\nAvailable variables:\n")
  message(strrep("-", 90))
  NextMethod("print")
  invisible(x)
}
