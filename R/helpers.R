.gmd_safe_get <- function(url) {
  tryCatch(
    {
      req <- httr2::req_error(httr2::request(url), is_error = function(resp) FALSE)
      response <- httr2::req_perform(req)
      status <- httr2::resp_status(response)
      if (status == 200) {
        response
      } else {
        message(sprintf("Request to %s returned HTTP status %d.", url, status))
        NULL
      }
    },
    error = function(e) {
      message(sprintf("Request to %s failed: %s", url, conditionMessage(e)))
      NULL
    }
  )
}

.gmd_load_versions_df <- function() {
  versions_url <- "https://gmd-releases.s3.ap-southeast-2.amazonaws.com/data/helpers/versions.csv"

  response <- .gmd_safe_get(versions_url)
  if (!is.null(response)) {
    versions_df <- readr::read_csv(
      httr2::resp_body_string(response, encoding = "UTF-8"),
      col_types = readr::cols(versions = readr::col_character())
    )
    if ("versions" %in% names(versions_df)) {
      return(versions_df)
    }
  }

  fallback_path <- system.file("versions.csv", package = "globalmacrodata")
  if (nzchar(fallback_path) && file.exists(fallback_path)) {
    message("Loading version list from local fallback.")
    return(readr::read_csv(fallback_path,
                           col_types = readr::cols(versions = readr::col_character())))
  }

  stop("Error: Unable to access version information. Check internet connection or reinstall the package.")
}

#' Get available versions of the Global Macro Database
#'
#' @return A character vector of available versions
#' @export
get_available_versions <- function() {
  versions_df <- .gmd_load_versions_df()
  if (!"versions" %in% names(versions_df)) {
    stop("Error: Version information is malformed.")
  }

  versions <- sort(unique(versions_df$versions), decreasing = TRUE)
  if (length(versions) == 0) {
    stop("Error: Version information is empty.")
  }

  versions
}

#' Get current version of the Global Macro Database
#'
#' @return A string representing the current version
#' @export
get_current_version <- function() {
  versions <- get_available_versions()
  versions[1]
}
