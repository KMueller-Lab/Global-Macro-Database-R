#' Find the most recent dataset available
#'
#' @param base_url The base URL where datasets are stored.
#' @return The latest dataset version as a string (e.g., "2025_03").
#' @export
find_latest_data <- function(base_url) {
  library(httr)

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  for (year in seq(current_year, 2020, by = -1)) {
    for (quarter in c("12", "09", "06", "03", "01")) {
      url <- paste0(base_url, "/GMD_", year, "_", quarter, ".csv")
      response <- HEAD(url, timeout(5))
      if (status_code(response) == 200) {
        return(paste0(year, "_", quarter))
      }
    }
  }

  stop("No available dataset found on the server.")
}
