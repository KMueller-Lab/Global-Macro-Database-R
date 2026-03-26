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

  # Required packages
  required_packages <- c("httr", "readr", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required. Please install it using install.packages('%s')", pkg, pkg))
    }
    library(pkg, character.only = TRUE)
  }

  # Base URL
  base_url <- "https://gmd-releases.s3.ap-southeast-2.amazonaws.com/data"

  # Display package information
  message("Global Macro Database by Müller, Xu, Lehbib, and Chen (2025)")
  message("Website: https://www.globalmacrodata.com")
  message("")

  # Load country mapping
  isomapping_path <- system.file("isomapping.csv", package = "globalmacrodata")
  if (!file.exists(isomapping_path)) {
    stop("Error: isomapping.csv not found in package installation")
  }
  country_mapping <- readr::read_csv(isomapping_path, show_col_types = FALSE)

  # Load variable list from CSV
  varlist_path <- system.file("varlist.csv", package = "globalmacrodata")
  if (!file.exists(varlist_path)) {
    stop("Error: varlist.csv not found in package installation")
  }
  varlist_df <- readr::read_csv(varlist_path, show_col_types = FALSE)
  valid_vars <- varlist_df$variables

  # Warning message
  if (iso && vars) {
    warning("You can only show either countries or variables, not both!")
    return(invisible(NULL))
  }

  if ((iso || vars) &&
      (!is.null(variables) || !is.null(country) || !is.null(version) || raw != FALSE)) {
    warning("When iso = TRUE or vars = TRUE, should not enter other inputs (variables, country, version, raw).")
  }

  # Handle ISO listing
  if (iso) {
    df <- data.frame(
      "Country_and_territories" = country_mapping$countryname,
      "Code" = country_mapping$ISO3,
      stringsAsFactors = FALSE
    )
    message("Loading countries list")
    return(df)
  }

  # Handle variable listing
  if (vars) {
    df <- data.frame(
      Variable = varlist_df$variables,
      Definition = varlist_df$definition,
      Units = varlist_df$units,
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
    cite_url <- paste0(base_url, "/helpers/bib_dataframe.csv")
    cite_response <- httr::GET(cite_url)
    if (httr::status_code(cite_response) != 200) {
      stop("Unable to import the list of sources to cite. Check internet connection.")
    }
    cite_df <- readr::read_csv(httr::content(cite_response, as = "text"), show_col_types = FALSE)

    if (tolower(cite) == "load") {
      message("Imported the list of sources to cite.")
      return(cite_df)
    } else {
      matched <- cite_df[tolower(cite_df$source) == tolower(cite), ]
      if (nrow(matched) == 0) {
        stop(sprintf("Source '%s' does not exist.\nTo load the list of sources to cite, use: gmd(cite = 'load')", cite))
      }
      message(matched$citation[1])
      return(invisible(matched))
    }
  }

  # ============================================================================
  # Version check
  # ============================================================================
  versions_url <- paste0(base_url, "/helpers/versions.csv")
  versions_response <- httr::GET(versions_url)

  # Fallback to GitHub if S3 is unavailable
  if (httr::status_code(versions_response) != 200) {
    versions_url <- "https://raw.githubusercontent.com/KMueller-Lab/Global-Macro-Database/refs/heads/main/data/helpers/versions.csv"
    versions_response <- httr::GET(versions_url)
    if (httr::status_code(versions_response) != 200) {
      stop("Error: Unable to access version information. Check internet connection.")
    }
  }

  versions_df <- readr::read_csv(httr::content(versions_response, as = "text"), show_col_types = FALSE)

  # Sort versions descending (YYYY_MM format) to get latest first
  versions_df <- versions_df[order(versions_df$versions, decreasing = TRUE), ]
  current_version <- versions_df$versions[1]
  available_versions <- versions_df$versions

  # Handle version = "list"
  if (!is.null(version) && tolower(version) == "list") {
    message("Available versions:")
    for (v in sort(available_versions)) {
      message(v)
    }
    return(invisible(available_versions))
  }

  # Handle version option
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

    # Load or list sources
    if (tolower(sources) %in% c("load", "list")) {
      source_list_url <- paste0(base_url, "/helpers/source_list.csv")
      source_response <- httr::GET(source_list_url)
      if (httr::status_code(source_response) != 200) {
        stop("Unable to load source list. Check internet connection.")
      }
      source_df <- readr::read_csv(httr::content(source_response, as = "text"), show_col_types = FALSE)

      if (tolower(sources) == "load") {
        message("Imported the list of sources.")
        return(source_df)
      } else {
        message("Available sources:")
        for (s in source_df$source_name) {
          message(s)
        }
        return(invisible(source_df$source_name))
      }
    }

    # Load a specific source dataset
    sources <- trimws(sources)
    if (length(strsplit(sources, "\\s+")[[1]]) > 1) {
      stop("Please specify exactly one source.")
    }

    source_url <- paste0(base_url, "/clean/combined/", sources, ".dta")
    source_response <- httr::GET(source_url)

    # If failed, try case-insensitive match
    if (httr::status_code(source_response) != 200) {
      source_list_url <- paste0(base_url, "/helpers/source_list.csv")
      sl_response <- httr::GET(source_list_url)
      if (httr::status_code(sl_response) != 200) {
        stop("Unable to access source list. Check internet connection.")
      }
      sl_df <- readr::read_csv(httr::content(sl_response, as = "text"), show_col_types = FALSE)
      matched_source <- sl_df$source_name[tolower(sl_df$source_name) == tolower(sources)]
      if (length(matched_source) == 1) {
        sources <- matched_source
        source_url <- paste0(base_url, "/clean/combined/", sources, ".dta")
        source_response <- httr::GET(source_url)
      }
      if (httr::status_code(source_response) != 200) {
        stop(sprintf("Invalid source name: %s\nTo see the list of sources, use: gmd(sources = 'list')", sources))
      }
    }

    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' is required for reading .dta files. Please install it using install.packages('haven')")
    }
    df <- haven::read_dta(httr::content(source_response, as = "raw"))

    # If variables specified, filter to matching source columns
    if (!is.null(variables)) {
      source_vars <- paste0(sources, "_", variables)
      existing <- intersect(source_vars, colnames(df))
      if (length(existing) == 0) {
        all_data_cols <- setdiff(colnames(df), c("ISO3", "year", "countryname", "id"))
        stop(sprintf("This source doesn't have data on %s. It has data on: %s",
                    paste(variables, collapse = ", "),
                    paste(gsub(paste0("^", sources, "_"), "", all_data_cols), collapse = ", ")))
      }
      id_cols <- intersect(c("ISO3", "year", "countryname", "id"), colnames(df))
      df <- df %>% dplyr::select(dplyr::all_of(c(id_cols, existing)))
    }

    # Filter by country
    if (!is.null(country)) {
      country <- toupper(country)
      invalid_countries <- country[!country %in% country_mapping$ISO3]
      if (length(invalid_countries) > 0) {
        stop(sprintf("Error: Invalid country code(s): %s\n\nTo see the list of valid country codes, use: gmd(iso = TRUE)",
                    paste(invalid_countries, collapse = ", ")))
      }
      df <- df %>% dplyr::filter(ISO3 %in% country)
    }

    df <- df %>% dplyr::arrange(ISO3, year)

    # Drop columns that are entirely NA
    all_na_cols <- colnames(df)[sapply(df, function(col) all(is.na(col)))]
    if (length(all_na_cols) > 0) {
      df <- df %>% dplyr::select(-dplyr::all_of(all_na_cols))
    }

    n_vars <- ncol(df) - length(intersect(c("ISO3", "year", "countryname", "id"), colnames(df)))
    message(sprintf("Final dataset: %d observations of %d variables", nrow(df), n_vars))
    message(sprintf("Version: %s", current_version))

    # Citation info
    message("")
    message("When using these data, please cite:")
    message("Müller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database.")
    message("To get BibTeX: gmd(cite = 'GMD')")

    return(df)
  }

  # ============================================================================
  # Validate variables
  # ============================================================================
  if (!is.null(variables)) {
    # Block identifying variables
    id_names <- c("iso3", "year", "id", "countryname")
    id_matches <- variables[tolower(variables) %in% id_names]
    if (length(id_matches) > 0) {
      stop(sprintf("%s is an identifying variable in the dataset, specify common variables.\nTo see the list of variables, use: gmd(vars = TRUE)",
                  paste(id_matches, collapse = ", ")))
    }

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
    data_url <- paste0(base_url, "/distribute/", variables, "_", current_version, ".csv")

    response <- httr::GET(data_url)
    if (httr::status_code(response) != 200) {
      # Check if variable is valid but has no raw data
      if (variables %in% valid_vars) {
        stop(sprintf("Variable '%s' does not have raw data.", variables))
      } else {
        stop(sprintf("Specified variable '%s' is not valid.\nTo see the list of variables, use: gmd(vars = TRUE)", variables))
      }
    }

    df <- readr::read_csv(httr::content(response, as = "text"), show_col_types = FALSE)

    # Filter by country if specified
    if (!is.null(country)) {
      country <- toupper(country)
      invalid_countries <- country[!country %in% country_mapping$ISO3]
      if (length(invalid_countries) > 0) {
        stop(sprintf("Error: Invalid country code(s): %s\n\nTo see the list of valid country codes, use: gmd(iso = TRUE)",
                    paste(invalid_countries, collapse = ", ")))
      }
      df <- df %>% dplyr::filter(ISO3 %in% country)
    }

    df <- df %>%
      dplyr::select(ISO3, countryname, year, dplyr::everything()) %>%
      dplyr::arrange(countryname, year)

    if (nrow(df) == 0) {
      stop("No data available for the specified parameters")
    }

    n_sources <- ncol(df) - 7
    message(sprintf("Final dataset: %d observations of %d sources", nrow(df), n_sources))
    message(sprintf("Version: %s", current_version))

    # Citation info
    message("")
    message("When using these data, please cite:")
    message("Müller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database.")
    message("To get BibTeX: gmd(cite = 'GMD')")

    return(df)
  }

  # ============================================================================
  # Main dataset
  # ============================================================================
  response <- httr::GET(data_url)
  if (httr::status_code(response) != 200) {
    stop(sprintf("Error: Data file not found at %s", data_url))
  }

  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' is required for reading .dta files. Please install it using install.packages('haven')")
  }
  df <- haven::read_dta(httr::content(response, as = "raw"))

  # Filter by country if specified
  if (!is.null(country)) {
    country <- toupper(country)
    invalid_countries <- country[!country %in% country_mapping$ISO3]
    if (length(invalid_countries) > 0) {
      stop(sprintf("Error: Invalid country code(s): %s\n\nTo see the list of valid country codes, use: gmd(iso = TRUE)",
                  paste(invalid_countries, collapse = ", ")))
    }
    df <- df %>% dplyr::filter(ISO3 %in% country)
    message(sprintf("Filtered data for countries: %s", paste(country, collapse = ", ")))
  }

  # Select variables if specified
  if (!is.null(variables)) {
    required_cols <- c("ISO3", "countryname", "year", "id")
    available_vars <- intersect(variables, colnames(df))

    if (length(available_vars) == 0) {
      warning("None of the requested variables are available in the dataset.")
    }

    df <- df %>% dplyr::select(dplyr::all_of(c(required_cols, available_vars)))

    # Drop rows where all specified variables are missing (matching Stata behavior)
    if (length(available_vars) > 0) {
      df <- df %>% dplyr::filter(
        rowSums(!is.na(dplyr::select(., dplyr::all_of(available_vars)))) > 0
      )
    }
  }

  # Drop columns that are entirely NA
  id_cols <- c("ISO3", "countryname", "year", "id")
  all_na_cols <- colnames(df)[sapply(df, function(col) all(is.na(col)))]
  all_na_cols <- setdiff(all_na_cols, id_cols)
  if (length(all_na_cols) > 0) {
    df <- df %>% dplyr::select(-dplyr::all_of(all_na_cols))
  }

  # Order and sort data
  df <- df %>%
    dplyr::select(ISO3, countryname, year, dplyr::everything()) %>%
    dplyr::arrange(countryname, year)

  # Check if we have any data
  n_vars <- ncol(df) - 4
  if (nrow(df) == 0 || n_vars <= 0) {
    stop("No data available for the specified parameters")
  }
  if (n_vars > 1) {
    message(sprintf("Final dataset: %d observations for %d variables", nrow(df), n_vars))
  } else {
    message(sprintf("Final dataset: %d observations for %d variable", nrow(df), n_vars))
  }
  message(sprintf("Version: %s", current_version))

  # Citation info
  message("")
  message("When using these data, please cite:")
  message("Müller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database.")
  message("To get BibTeX: gmd(cite = 'GMD')")

  return(df)
}

# Print rule
print.gmd_vars <- function(x, ...) {
  message("\nAvailable variables:\n")
  message(strrep("-", 90))
  NextMethod("print")
  invisible(x)
}
