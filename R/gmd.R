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
#'   or \code{c("rGDP", "unemp")}). Can also be used with \code{sources} to load
#'   specific variables from a given source.
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
                sources = NULL, cite = NULL) {

  base_url <- "https://gmd-releases.s3.ap-southeast-2.amazonaws.com/data"
  ID_COLS <- c("ISO3", "year", "countryname", "id")

  message("Global Macro Database by M\u00fcller, Xu, Lehbib, and Chen (2025)")
  message("Website: https://www.globalmacrodata.com")
  message("")

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
      return(haven::read_dta(httr::content(resp, as = "raw")))
    }
    # Fallback to bundled CSV
    path <- system.file("isomapping.csv", package = "globalmacrodata")
    if (nzchar(path) && file.exists(path)) {
      message("Loading country list from local fallback.")
      return(readr::read_csv(path, show_col_types = FALSE))
    }
    stop("Unable to load country list. Check internet connection or reinstall the package.")
  }

  load_varlist <- function() {
    resp <- .gmd_safe_get(paste0(base_url, "/helpers/varlist.csv"))
    if (!is.null(resp)) {
      return(readr::read_csv(httr::content(resp, as = "text", encoding = "UTF-8"), show_col_types = FALSE))
    }
    # Fallback to bundled CSV
    path <- system.file("varlist.csv", package = "globalmacrodata")
    if (nzchar(path) && file.exists(path)) {
      message("Loading variable list from local fallback.")
      return(readr::read_csv(path, show_col_types = FALSE))
    }
    stop("Unable to load variable list. Check internet connection or reinstall the package.")
  }

  validate_country <- function(country, country_mapping) {
    country <- toupper(country)
    invalid <- country[!country %in% country_mapping$ISO3]
    if (length(invalid) > 0) {
      stop(sprintf("Error: Invalid country code(s): %s\n\nTo see the list of valid country codes, use: gmd(iso = TRUE)",
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
      (!is.null(variables) || !is.null(country) || !is.null(version) || raw != FALSE)) {
    warning("When iso = TRUE or vars = TRUE, should not enter other inputs (variables, country, version, raw).")
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
    cite_resp <- .gmd_safe_get(paste0(base_url, "/helpers/bib_dataframe.csv"))
    if (is.null(cite_resp)) {
      stop("Unable to import the list of sources to cite. Check internet connection.")
    }
    cite_df <- readr::read_csv(httr::content(cite_resp, as = "text", encoding = "UTF-8"), show_col_types = FALSE)

    if (tolower(cite) == "load") {
      message("Imported the list of sources to cite.")
      return(cite_df)
    }

    matched <- cite_df[tolower(cite_df$source_name) == tolower(cite), ]
    if (nrow(matched) == 0) {
      stop(sprintf("Source '%s' does not exist.\nTo load the list of sources to cite, use: gmd(cite = 'load')", cite))
    }
    message(matched$citation[1])
    return(invisible(matched))
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
        stop(sprintf("Error: %s is not valid\nAvailable versions are: %s\nThe current version is: %s",
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
      source_df <- readr::read_csv(httr::content(source_resp, as = "text", encoding = "UTF-8"), show_col_types = FALSE)

      if (tolower(sources) == "load") {
        message("Imported the list of sources.")
        return(source_df)
      }
      message("Available sources:")
      for (s in source_df$source_name) message(s)
      return(invisible(source_df$source_name))
    }

    sources <- trimws(sources)
    source_resp <- .gmd_safe_get(paste0(base_url, "/clean/combined/", sources, ".dta"))

    # Case-insensitive fallback
    if (is.null(source_resp)) {
      sl_resp <- .gmd_safe_get(paste0(base_url, "/helpers/source_list.csv"))
      if (is.null(sl_resp)) {
        stop("Unable to access source list. Check internet connection.")
      }
      sl_df <- readr::read_csv(httr::content(sl_resp, as = "text", encoding = "UTF-8"), show_col_types = FALSE)
      matched_source <- sl_df$source_name[tolower(sl_df$source_name) == tolower(sources)]
      if (length(matched_source) == 1) {
        sources <- matched_source
        source_resp <- .gmd_safe_get(paste0(base_url, "/clean/combined/", sources, ".dta"))
      }
      if (is.null(source_resp)) {
        stop(sprintf("Invalid source name: %s\nTo see the list of sources, use: gmd(sources = 'list')", sources))
      }
    }

    require_haven()
    df <- haven::read_dta(httr::content(source_resp, as = "raw"))

    if (!is.null(variables)) {
      source_vars <- paste0(sources, "_", variables)
      existing <- intersect(source_vars, colnames(df))
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

    df <- df[order(df$ISO3, df$year), ]
    df <- drop_na_cols(df)

    n_vars <- ncol(df) - length(intersect(ID_COLS, colnames(df)))
    message(sprintf("Final dataset: %d observations of %d variables", nrow(df), n_vars))
    print_citation(current_version)
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
    invalid_vars <- setdiff(variables, valid_vars)
    if (length(invalid_vars) > 0) {
      stop(sprintf("Invalid variable code(s): %s\n\nTo see the list of valid variable codes, use: gmd(vars = TRUE)",
                  paste(invalid_vars, collapse = ", ")))
    }
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

    df <- readr::read_csv(httr::content(raw_resp, as = "text", encoding = "UTF-8"), show_col_types = FALSE)

    if (!is.null(country)) {
      country <- validate_country(country, get_country_mapping())
      df <- df[df$ISO3 %in% country, , drop = FALSE]
    }

    df <- reorder_cols(df)
    df <- df[order(df$countryname, df$year), ]

    if (nrow(df) == 0) stop("No data available for the specified parameters")

    n_sources <- ncol(df) - length(intersect(ID_COLS, colnames(df)))
    message(sprintf("Final dataset: %d observations of %d sources", nrow(df), n_sources))
    print_citation(current_version)
    return(df)
  }

  # ============================================================================
  # Main dataset
  # ============================================================================
  require_haven()
  main_resp <- .gmd_safe_get(data_url)
  if (is.null(main_resp)) {
    stop(sprintf("Error: Data file not found at %s\nCheck internet connection.", data_url))
  }
  df <- haven::read_dta(httr::content(main_resp, as = "raw"))

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

  return(df)
}

#' @export
print.gmd_vars <- function(x, ...) {
  message("\nAvailable variables:\n")
  message(strrep("-", 90))
  NextMethod("print")
  invisible(x)
}
