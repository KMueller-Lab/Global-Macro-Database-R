#' Get macroeconomic data from Global Macro Data
#'
#' @param version A string representing the dataset version (e.g., "2025_01").
#' @param country A string or vector of ISO3 country codes (e.g., "USA" or c("CHN", "USA")).
#' @param variables A vector of variable names to include.
#' @param show_preview A boolean indicating whether to show a preview.
#' @return A dataframe containing the requested macroeconomic data.
#' @export
gmd <- function(version = NULL, country = NULL, variables = NULL, show_preview = TRUE) {

  library(httr)
  library(readr)
  library(dplyr)

  base_url <- "https://www.globalmacrodata.com"
  
  if (is.null(version)) {
    version <- find_latest_data(base_url)
  } else {
    show_preview <- FALSE
    if (!grepl("^\\d{4}_(01|03|06|09|12)$", version)) {
      stop("Version must be in format 'YYYY_MM' where MM is one of: 01, 03, 06, 09, 12")
    }
  }

  data_url <- paste0(base_url, "/GMD_", version, ".csv")
  message("Downloading: ", data_url)

  # download data
  response <- httr::GET(data_url)
  if (httr::status_code(response) != 200) {
    stop("Error: Data file not found at ", data_url)
  }

  df <- readr::read_csv(httr::content(response, as = "text"))

  # check iso3 code
  script_dir <- system.file(package = "globalmacrodata")
  isomapping_path <- file.path(script_dir, "isomapping.csv")
  if (file.exists(isomapping_path)) {
    isomapping <- readr::read_csv(isomapping_path, show_col_types = FALSE)
  } else {
    isomapping <- NULL
  }

  # countries
  if (!is.null(country)) {
    country <- toupper(country)  
    invalid_countries <- setdiff(country, unique(df$ISO3))

    if (length(invalid_countries) > 0) {
      message("Error: Invalid country code(s): ", paste(invalid_countries, collapse = ", "))

      if (!is.null(isomapping)) {
        message("Available country codes from isomapping.csv:")
        print(isomapping)
      } else {
        message("Available country codes from dataset:")
        print(unique(df[, c("ISO3", "countryname")]))
      }

      stop("Invalid country code(s): ", paste(invalid_countries, collapse = ", "))
    }

    df <- df %>% dplyr::filter(ISO3 %in% country)
    message("Filtered data for countries: ", paste(country, collapse = ", "))
  }

  # variables
  if (!is.null(variables)) {
    required_cols <- c("ISO3", "countryname", "year")
    available_vars <- intersect(variables, colnames(df))

    if (length(available_vars) == 0) {
      warning("None of the requested variables are available in the dataset.")
    }

    df <- df %>% dplyr::select(dplyr::all_of(c(required_cols, available_vars)))
  }

  # Preview functionality for default call
  default_call <- show_preview && is.null(country) && is.null(variables)

  if (default_call) {
    message("Singapore (SGP) data, 2000-2020")

    # Filter Singapore (SGP) data for 2000-2020
    sample_df <- df[df$ISO3 == "SGP" & df$year >= 2000 & df$year <= 2020, , drop = FALSE]

    if (nrow(sample_df) > 0) {
      message(nrow(sample_df), " rows out of ", nrow(df), " total rows in the dataset")

      # Define preview columns in exact order as in Python
      preview_cols <- c("year", "ISO3", "countryname", "nGDP", "rGDP", "pop", "unemp", "infl", 
                        "exports", "imports", "govdebt_GDP", "ltrate")

      # Filter to available columns
      available_cols <- preview_cols[preview_cols %in% colnames(sample_df)]

      # Sort by year and select preview columns
      sample_df <- sample_df %>% 
        dplyr::arrange(year) %>% 
        dplyr::select(dplyr::all_of(available_cols))

      # Print the preview data explicitly (mimicking Python's print)
      print(sample_df)

      # Return only the preview data for default call
      return(sample_df)
    } else {
      message("No data available for Singapore (SGP) between 2000-2020")
      return(NULL)
    }
  }

  message("Final dataset: ", nrow(df), " observations of ", ncol(df), " variables")
  flush.console()
  return(df)
}
