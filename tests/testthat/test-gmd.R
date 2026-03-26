# Test suite for gmd function

library(testthat)
devtools::load_all("C:/Users/shixu/Documents/GitHub/Global-Macro-Database-R")

# ==============================================================================
# Basic functionality
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

  df <- gmd(version = "2026_01")
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
  # Filtered dataset should have fewer or equal rows (no all-NA rows)
  expect_lte(nrow(df_var), nrow(df_full))
  # All rows should have at least one non-NA value in rGDP
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
# Variable and country listing
# ==============================================================================

test_that("list variables returns dataframe with definitions and units", {
  skip_on_cran()

  df <- gmd(vars = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Variable", "Definition", "Units") %in% names(df)))
  expect_gt(nrow(df), 0)
  expect_true("rGDP" %in% df$Variable)
})

test_that("list countries works", {
  skip_on_cran()

  df <- gmd(iso = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Country_and_territories", "Code") %in% names(df)))
  expect_gt(nrow(df), 0)
  expect_true("USA" %in% df$Code)
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
  expect_true("source_name" %in% names(df))
  expect_true("citation" %in% names(df))
  expect_gt(nrow(df), 0)
})

test_that("cite specific source works", {
  skip_on_cran()
  skip_if_offline()

  result <- gmd(cite = "GMD")
  # Returns invisibly
  expect_s3_class(result, "data.frame")
})

# ==============================================================================
# Combined parameters
# ==============================================================================

test_that("combined parameters work", {
  skip_on_cran()
  skip_if_offline()

  df <- gmd(
    version = "2026_01",
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

test_that("identifying variable is blocked", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(variables = "ISO3"), "identifying variable")
  expect_error(gmd(variables = "year"), "identifying variable")
})

test_that("raw with multiple variables fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(variables = c("rGDP", "infl"), raw = TRUE))
})

test_that("raw without variable fails", {
  skip_on_cran()
  skip_if_offline()

  expect_error(gmd(raw = TRUE))
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

test_that("iso and vars together warns", {
  skip_on_cran()

  expect_warning(gmd(iso = TRUE, vars = TRUE))
})
