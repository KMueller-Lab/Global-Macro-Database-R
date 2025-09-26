# Test suite for gmd function

library(testthat)
library(globalmacrodata)

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
  
  df <- gmd(version = "2025_01")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

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

test_that("specific variables work", {
  skip_on_cran()
  skip_if_offline()
  
  df <- gmd(variables = c("rGDP", "infl", "unemp"))
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true(all(c("rGDP", "infl", "unemp") %in% names(df)))
})

test_that("raw data works", {
  skip_on_cran()
  skip_if_offline()
  
  df <- gmd(variables = "rGDP", raw = TRUE)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_true("rGDP" %in% names(df))
})

test_that("list variables works", {
  skip_on_cran()
  skip_if_offline()
  
  output <- capture.output(gmd(vars = TRUE))
  expect_true(any(grepl("Variable", output)))
  expect_true(any(grepl("Description", output)))
})

test_that("list countries works", {
  skip_on_cran()
  skip_if_offline()
  
  output <- capture.output(gmd(iso = TRUE))
  expect_true(any(grepl("Country_and_territories", output)))
  expect_true(any(grepl("Code", output)))
})

test_that("combined parameters work", {
  skip_on_cran()
  skip_if_offline()
  
  df <- gmd(
    version = "2025_01",
    country = c("USA", "CHN"),
    variables = c("rGDP", "unemp", "CPI")
  )
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_equal(sort(unique(df$ISO3)), c("CHN", "USA"))
  expect_true(all(c("rGDP", "unemp", "CPI") %in% names(df)))
})

# Error cases
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
