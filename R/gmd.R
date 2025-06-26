#' Get macroeconomic data from Global Macro Data
#'
#' @param variables A character vector of variable names to include
#' @param country A string representing the ISO3 country code
#' @param version A string representing the dataset version (e.g., "2025_01")
#' @param raw A logical indicating whether to return raw data
#' @param iso A logical indicating whether to show ISO country codes
#' @param vars A logical indicating whether to show available variables
#' @return A dataframe containing the requested macroeconomic data
#' @export
gmd <- function(variables = NULL, country = NULL, version = NULL, 
                raw = FALSE, iso = FALSE, vars = FALSE) {
  
  # Required packages
  required_packages <- c("httr", "readr", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required. Please install it using install.packages('%s')", pkg, pkg))
    }
    # Actually load the package
    library(pkg, character.only = TRUE)
  }
  
  # Base URL
  base_url <- "https://www.globalmacrodata.com"
  
  # Display package information
  message("Global Macro Database by Müller et. al (2025)")
  message("Website: https://www.globalmacrodata.com")
  message("")
  
  # Get current version
  versions_url <- "https://raw.githubusercontent.com/KMueller-Lab/Global-Macro-Database/refs/heads/main/data/helpers/versions.csv"
  versions_response <- httr::GET(versions_url)
  if (httr::status_code(versions_response) != 200) {
    stop("Error: Unable to access version information. Check internet connection.")
  }
  
  versions_df <- readr::read_csv(httr::content(versions_response, as = "text"), show_col_types = FALSE)
  
  current_version <- versions_df$versions[nrow(versions_df)]
  available_versions <- versions_df$versions
  
  # Handle version option
  if (!is.null(version)) {
    if (tolower(version) == "current") {
      data_url <- paste0(base_url, "/GMD_", current_version, ".dta")
    } else {
      if (!version %in% available_versions) {
        stop(sprintf("Error: %s is not valid\nAvailable versions are: %s\nThe current version is: %s",
                    version, paste(available_versions, collapse = ", "), current_version))
      }
      data_url <- paste0(base_url, "/GMD_", version, ".dta")
      current_version <- version
    }
  } else {
    data_url <- paste0(base_url, "/GMD_", current_version, ".dta")
  }
  
  # Load country mapping
  isomapping_path <- system.file("isomapping.csv", package = "globalmacrodata")
  if (!file.exists(isomapping_path)) {
    stop("Error: isomapping.csv not found in package installation")
  }
  country_mapping <- readr::read_csv(isomapping_path, show_col_types = FALSE)
  
  # Validate variables if specified
  if (!is.null(variables)) {
    valid_vars <- c("nGDP", "rGDP", "rGDP_USD", "rGDP_pc", "deflator", "cons", "cons_GDP", 
                   "rcons", "inv", "inv_GDP", "finv", "finv_GDP", "exports", "exports_GDP", 
                   "imports", "imports_GDP", "CA", "CA_GDP", "USDfx", "REER", "govexp", 
                   "govexp_GDP", "govrev", "govrev_GDP", "govtax", "govtax_GDP", "govdef", 
                   "govdef_GDP", "govdebt", "govdebt_GDP", "HPI", "CPI", "infl", "pop", 
                   "unemp", "strate", "ltrate", "cbrate", "M0", "M1", "M2", "M3", "M4", 
                   "CurrencyCrisis", "BankingCrisis", "SovDebtCrisis")
    
    invalid_vars <- setdiff(variables, valid_vars)
    if (length(invalid_vars) > 0) {
      stop(sprintf("Invalid variable code(s): %s\n\nTo see the list of valid variable codes, use: list_iso_vars(vars = TRUE)",
                  paste(invalid_vars, collapse = ", ")))
    }
  }
  
  # Handle raw data option
  if (raw) {
    if (length(variables) != 1) {
      stop("Warning: raw requires specifying exactly one variable (not more, not less).")
    }
    
    if (is.null(variables)) {
      stop("Warning: No variable specified.\nNote: Raw data is only accessed variable-wise using: gmd(variables = 'var_name', raw = TRUE)")
    }
    
    message(sprintf("Importing raw data for variable: %s", variables))
    data_url <- paste0(base_url, "/", variables, "_", current_version, ".csv")
  }
  
  # Download and process data
  response <- httr::GET(data_url)
  if (httr::status_code(response) != 200) {
    stop(sprintf("Error: Data file not found at %s", data_url))
  }
  
  # Read data based on file type
  if (grepl("\\.csv$", data_url)) {
    df <- readr::read_csv(httr::content(response, as = "text"), show_col_types = FALSE)
  } else {
    # For .dta files, we'll need the haven package
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' is required for reading .dta files. Please install it using install.packages('haven')")
    }
    df <- haven::read_dta(httr::content(response, as = "raw"))
  }
  
  # Filter by country if specified
  if (!is.null(country)) {
    country <- toupper(country)
    invalid_countries <- country[!country %in% country_mapping$ISO3]
    if (length(invalid_countries) > 0) {
      stop(sprintf("Error: Invalid country code(s): %s\n\nTo see the list of valid country codes, use: list_iso_vars(iso = TRUE)",
                  paste(invalid_countries, collapse = ", ")))
    }
    
    df <- df %>% dplyr::filter(ISO3 %in% country)
    message(sprintf("Filtered data for countries: %s", paste(country, collapse = ", ")))
  }
  
  # Select variables if specified
  if (!is.null(variables)) {
    required_cols <- c("ISO3", "countryname", "year")
    available_vars <- intersect(variables, colnames(df))
    
    if (length(available_vars) == 0) {
      warning("None of the requested variables are available in the dataset.")
    }
    
    df <- df %>% dplyr::select(dplyr::all_of(c(required_cols, available_vars)))
  }
  
  # Order and sort data
  df <- df %>%
    dplyr::select(ISO3, countryname, year, dplyr::everything()) %>%
    dplyr::arrange(countryname, year)
  
  # Check if we have any data
  if (nrow(df) == 0) {
    stop(sprintf("No data available for the specified parameters"))
  }
  
  # Display final dataset dimensions
  if (nrow(df) > 0) {
    if (raw) {
      n_sources <- ncol(df) - 8
      message(sprintf("Final dataset: %d observations of %d sources", nrow(df), n_sources))
    } else {
      message(sprintf("Final dataset: %d observations of %d variables", nrow(df), ncol(df)))
    }
    
    message(sprintf("Version: %s", current_version))
  }
  
  return(df)
}
list_iso_vars <- function(iso = FALSE, vars = FALSE){
  # Base URL
  base_url <- "https://www.globalmacrodata.com"
  
  # Display package information
  message("Global Macro Database by Müller et. al (2025)")
  message("Website: https://www.globalmacrodata.com")
  message("")
  
  # Load country mapping
  isomapping_path <- system.file("isomapping.csv", package = "globalmacrodata")
  if (!file.exists(isomapping_path)) {
    stop("Error: isomapping.csv not found in package installation")
  }
  country_mapping <- readr::read_csv(isomapping_path, show_col_types = FALSE)
  
  # Warnimng message
  if (iso && vars) {
    warning("You can only show either countries or variables, not both!")
    return(invisible(NULL))
  }
  if (!iso && !vars) {
    warning("You must set either iso or vars to TRUE to display information!")
    return(invisible(NULL))
  }
  
  # Handle ISO listing
  if (iso) {
    message("Country and territories", strrep(" ", 30), "Code")
    message(strrep("-", 60))
    for (i in seq_len(nrow(country_mapping))) {
      message(sprintf("%-45s %s", country_mapping$countryname[i], country_mapping$ISO3[i]))
    }
    message(strrep("-", 60))
    return(invisible(NULL))
  }
  
  # Handle variable listing
  if (vars) {
    message("\nAvailable variables:\n")
    message(strrep("-", 90))
    message(sprintf("%-15s %s", "Variable", "Description"))
    message(strrep("-", 90))
    
    var_descriptions <- list(
      "nGDP" = "Nominal Gross Domestic Product",
      "rGDP" = "Real Gross Domestic Product, in 2010 prices",
      "rGDP_pc" = "Real Gross Domestic Product per Capita",
      "rGDP_USD" = "Real Gross Domestic Product in USD",
      "deflator" = "GDP deflator",
      "cons" = "Total Consumption",
      "rcons" = "Real Total Consumption",
      "cons_GDP" = "Total Consumption as % of GDP",
      "inv" = "Total Investment",
      "inv_GDP" = "Total Investment as % of GDP",
      "finv" = "Fixed Investment",
      "finv_GDP" = "Fixed Investment as % of GDP",
      "exports" = "Total Exports",
      "exports_GDP" = "Total Exports as % of GDP",
      "imports" = "Total Imports",
      "imports_GDP" = "Total Imports as % of GDP",
      "CA" = "Current Account Balance",
      "CA_GDP" = "Current Account Balance as % of GDP",
      "USDfx" = "Exchange Rate against USD",
      "REER" = "Real Effective Exchange Rate, 2010 = 100",
      "govexp" = "Government Expenditure",
      "govexp_GDP" = "Government Expenditure as % of GDP",
      "govrev" = "Government Revenue",
      "govrev_GDP" = "Government Revenue as % of GDP",
      "govtax" = "Government Tax Revenue",
      "govtax_GDP" = "Government Tax Revenue as % of GDP",
      "govdef" = "Government Deficit",
      "govdef_GDP" = "Government Deficit as % of GDP",
      "govdebt" = "Government Debt",
      "govdebt_GDP" = "Government Debt as % of GDP",
      "HPI" = "House Price Index",
      "CPI" = "Consumer Price Index, 2010 = 100",
      "infl" = "Inflation Rate",
      "pop" = "Population",
      "unemp" = "Unemployment Rate",
      "strate" = "Short-term Interest Rate",
      "ltrate" = "Long-term Interest Rate",
      "cbrate" = "Central Bank Policy Rate",
      "M0" = "M0 Money Supply",
      "M1" = "M1 Money Supply",
      "M2" = "M2 Money Supply",
      "M3" = "M3 Money Supply",
      "M4" = "M4 Money Supply",
      "SovDebtCrisis" = "Sovereign Debt Crisis",
      "CurrencyCrisis" = "Currency Crisis",
      "BankingCrisis" = "Banking Crisis"
    )
    
    for (var in names(var_descriptions)) {
      message(sprintf("%-15s %s", var, var_descriptions[[var]]))
    }
    message(strrep("-", 90))
    return(invisible(NULL))
  }
}