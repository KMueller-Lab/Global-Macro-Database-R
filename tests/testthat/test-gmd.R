# Test suite for gmd function

library(testthat)
library(globalmacrodata)

# ==============================================================================
# Offline fallback tests (no skip_if_offline — these MUST pass without network)
# ==============================================================================

test_that("vars=TRUE works offline via bundled fallback", {
  skip_on_cran()

  df <- gmd(vars = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Variable", "Definition", "Units") %in% names(df)))
  expect_gt(nrow(df), 0)
  expect_true("rGDP" %in% df$Variable)
})

test_that("iso=TRUE works offline via bundled fallback", {
  skip_on_cran()

  df <- gmd(iso = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Country_and_territories", "Code") %in% names(df)))
  expect_gt(nrow(df), 0)
  expect_true("USA" %in% df$Code)
})

test_that("version list works offline via bundled fallback", {
  skip_on_cran()

  versions <- gmd(version = "list")
  expect_true(is.character(versions))
  expect_gt(length(versions), 0)
  expect_true("2025_01" %in% versions)
})

test_that("get_available_versions works offline via bundled fallback", {
  skip_on_cran()

  versions <- get_available_versions()
  expect_true(is.character(versions))
  expect_gt(length(versions), 0)
  expect_true("2025_01" %in% versions)
})

test_that("get_current_version works offline via bundled fallback", {
  skip_on_cran()

  current_version <- get_current_version()
  expect_true(is.character(current_version))
  expect_length(current_version, 1)
  expect_identical(current_version, get_available_versions()[1])
})

test_that("iso and vars together warns without network", {
  skip_on_cran()

  expect_warning(gmd(iso = TRUE, vars = TRUE))
})

test_that("identifying variable is blocked without network", {
  skip_on_cran()

  expect_error(gmd(variables = "ISO3"), "identifying variable")
  expect_error(gmd(variables = "year"), "identifying variable")
})

test_that("raw without variable fails without network", {
  skip_on_cran()

  expect_error(gmd(raw = TRUE))
})

test_that("raw with multiple variables fails without network", {
  skip_on_cran()

  expect_error(gmd(variables = c("rGDP", "infl"), raw = TRUE))
})

test_that("CS source names map to <ISO3>_<n> files and CS<n> column prefixes", {
  source_names <- globalmacrodata:::.gmd_source_names

  expect_identical(source_names("CS1_ARG"), list(file = "ARG_1", prefix = "CS1"))
  expect_identical(source_names("CS10_ITA"), list(file = "ITA_10", prefix = "CS10"))
  expect_identical(source_names("cs10_ita"), list(file = "ITA_10", prefix = "CS10"))
  expect_identical(source_names("CS123_usa"), list(file = "USA_123", prefix = "CS123"))
  for (s in c("IMF_WEO", "AAL", "CatSol", "Mitchell", "BoCBoE", "CEPII",
              "ITA_10", "ARG_1", "CS_ARG", "CS1_AR", "CS1_ARGX")) {
    expect_identical(source_names(s), list(file = s, prefix = s))
  }
})

test_that("CS source loads <ISO3>_<n>.dta and selects CS<n>_ columns", {
  skip_on_cran()
  skip_if_not_installed("testthat", "3.1.7") # local_mocked_bindings()

  path <- tempfile(fileext = ".dta")
  haven::write_dta(data.frame(ISO3 = "ITA", year = 1900, CS10_CPI = 1, CS10_nGDP = 2), path)
  body <- readBin(path, "raw", file.size(path))
  unlink(path)
  requested <- character(0)
  local_mocked_bindings(
    .gmd_safe_get = function(url, quiet = FALSE) {
      requested <<- c(requested, url)
      if (endsWith(url, "/clean/combined/ITA_10.dta")) httr2::response(200, body = body) else NULL
    },
    .package = "globalmacrodata"
  )

  df <- gmd(sources = "cs10_ita", variables = "cpi")
  expect_identical(names(df), c("ISO3", "year", "CS10_CPI"))
  expect_false(any(grepl("CS10_ITA", requested, ignore.case = TRUE)))
  expect_error(gmd(sources = "CS10_ITA", variables = "rGDP"), "It has data on: CPI, nGDP")
})

# ==============================================================================
# Online tests — basic functionality
# ==============================================================================

test_that("default call returns data", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd()
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("ISO3", "countryname", "year") %in% names(df)))
})

test_that("specific version works", {
  skip_on_cran()
  skip_if_offline()

  # An older pinned vintage (S3 now serves all listed versions)
  df <- gmd(version = "2025_09")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("version current works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(version = "current")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("version list returns available versions", {
  skip_on_cran()
  skip_if_offline()

  result <- gmd(version = "list")
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true("2025_01" %in% result)
})

# ==============================================================================
# Country filtering
# ==============================================================================

test_that("specific country works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(country = "USA")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(df$ISO3 == "USA"))
})

test_that("multiple countries work", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(country = c("USA", "CHN", "DEU"))
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_equal(sort(unique(df$ISO3)), c("CHN", "DEU", "USA"))
})

# ==============================================================================
# Variable selection
# ==============================================================================

test_that("specific variables work", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(variables = c("rGDP", "infl", "unemp"))
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("rGDP", "infl", "unemp") %in% names(df)))
})

test_that("rows with all specified variables missing are dropped", {
  skip_on_cran()
  skip_if_offline()

  df_full <- gmd(country = "USA")
  df_var <- gmd(country = "USA", variables = "rGDP")
  expect_lte(nrow(df_var), nrow(df_full))
  expect_true(all(!is.na(df_var$rGDP)))
})

# ==============================================================================
# Raw data
# ==============================================================================

test_that("raw data works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(variables = "rGDP", raw = TRUE)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

# ==============================================================================
# Sources
# ==============================================================================

test_that("sources list works", {
  skip_on_cran()
  skip_if_offline()

  result <- gmd(sources = "list")
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("sources load works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(sources = "load")
  expect_s3_class(df, "data.frame")
  expect_true("source_name" %in% names(df))
  expect_gt(nrow(df), 0)
})

test_that("specific source works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(sources = "IMF_WEO")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true("ISO3" %in% names(df))
})

test_that("source with variables works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(sources = "IMF_WEO", variables = "nGDP")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("CS source names work for any slot number", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(sources = "CS10_ITA")
  expect_gt(nrow(df), 0)
  expect_true("CS10_CPI" %in% names(df))
  expect_identical(dim(gmd(sources = "cs10_ita")), dim(df))
  expect_identical(dim(gmd(sources = "ITA_10")), dim(df))
  expect_gt(nrow(gmd(sources = "CS1_ARG")), 0)

  df_cpi <- gmd(sources = "CS10_ITA", variables = "CPI")
  expect_true("CS10_CPI" %in% names(df_cpi))
  expect_false("CS10_nGDP" %in% names(df_cpi))
})

test_that("source with country filter works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(sources = "IMF_WEO", country = "USA")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(df$ISO3 == "USA"))
})

# ==============================================================================
# Cite
# ==============================================================================

test_that("cite load works", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(cite = "load")
  expect_s3_class(df, "data.frame")
  expect_true("source" %in% names(df))
  expect_true("citation" %in% names(df))
  expect_gt(nrow(df), 0)
})

test_that("cite specific source works", {
  skip_on_cran()
  skip_if_offline()

  result <- gmd(cite = "GMD")
  expect_s3_class(result, "data.frame")
})

# ==============================================================================
# Combined parameters
# ==============================================================================

test_that("combined parameters work", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(
    country = c("USA", "CHN"),
    variables = c("rGDP", "unemp", "CPI")
  )
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_equal(sort(unique(df$ISO3)), c("CHN", "USA"))
  expect_true(all(c("rGDP", "unemp", "CPI") %in% names(df)))
})

# ==============================================================================
# Error cases
# ==============================================================================

test_that("invalid version fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(version = "invalid_version"))
})

test_that("invalid country fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(country = "INVALID"))
})

test_that("invalid variable fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(variables = "INVALID"))
})

test_that("invalid source name fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(sources = "NONEXISTENT_SOURCE"))
})

test_that("invalid cite source fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(cite = "NONEXISTENT"))
})

# ==============================================================================
# Robustness fixes (#318) — input validation (offline, no network needed)
# ==============================================================================

test_that("version = NA gives a clear error, not a cryptic one (#2)", {
  skip_on_cran()

  expect_error(gmd(version = NA, country = "USA", variables = "rGDP"), "version")
  expect_error(gmd(version = NA_character_, country = "USA"), "version")
})

test_that("empty character vectors are rejected before any download (#6)", {
  skip_on_cran()

  expect_error(gmd(country = character(0), variables = "rGDP"), "country")
  expect_error(gmd(variables = character(0), country = "USA"), "variables")
})

test_that("error messages are not double-prefixed with 'Error:' (#3)", {
  skip_on_cran()

  msg <- tryCatch(gmd(version = "nope"), error = function(e) conditionMessage(e))
  expect_false(grepl("^Error:", msg))
  expect_match(msg, "is not valid")
})

test_that("iso = TRUE with other inputs errors instead of silently dropping them (#4)", {
  skip_on_cran()

  expect_error(gmd(iso = TRUE, country = "USA"))
  expect_error(gmd(vars = TRUE, variables = "rGDP"))
})

test_that("start_year / end_year are validated (#7)", {
  skip_on_cran()

  expect_error(gmd(country = "USA", start_year = "abc"), "start_year")
  expect_error(gmd(country = "USA", start_year = 2010, end_year = 2000), "end_year")
})

# ==============================================================================
# Robustness fixes (#318) — online behavior
# ==============================================================================

test_that("variable matching is case-insensitive (#5)", {
  skip_on_cran()
  skip_if_offline()

  df_lower <- gmd(variables = "rgdp", country = "USA", version = "2025_09")
  df_canon <- gmd(variables = "rGDP", country = "USA", version = "2025_09")
  expect_true("rGDP" %in% names(df_lower))
  expect_identical(sort(names(df_lower)), sort(names(df_canon)))
  expect_equal(nrow(df_lower), nrow(df_canon))
})

test_that("start_year and end_year filter the year range (#7)", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(country = "USA", variables = "rGDP", start_year = 2000, end_year = 2010)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_gte(min(df$year), 2000)
  expect_lte(max(df$year), 2010)
})
