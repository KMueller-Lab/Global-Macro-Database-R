#' Get available versions of the Global Macro Database
#'
#' @return A character vector of available versions
#' @export
get_available_versions <- function() {
  versions_url <- "https://raw.githubusercontent.com/KMueller-Lab/Global-Macro-Database/refs/heads/main/data/helpers/versions.csv"
  response <- httr::GET(versions_url)
  if (httr::status_code(response) != 200) {
    stop("Error: Unable to access version information. Check internet connection.")
  }
  
  versions_df <- readr::read_csv(httr::content(response, as = "text"), show_col_types = FALSE)
  return(versions_df$versions)
}

#' Get current version of the Global Macro Database
#'
#' @return A string representing the current version
#' @export
get_current_version <- function() {
  versions <- get_available_versions()
  return(versions[1])
} 