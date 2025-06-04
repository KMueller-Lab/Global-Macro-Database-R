# Simple test script for README examples
library(devtools)
library(testthat)

# Load the package
load_all()

# Run the examples from README
cat("\nTesting default call...\n")
df <- gmd()
print(head(df))

cat("\nTesting specific version...\n")
df <- gmd(version = "2025_01")
print(head(df))

cat("\nTesting specific country...\n")
df <- gmd(country = "USA")
print(head(df))

cat("\nTesting multiple countries...\n")
df <- gmd(country = c("USA", "CHN", "DEU"))
print(head(df))

cat("\nTesting specific variables...\n")
df <- gmd(variables = c("rGDP", "infl", "unemp"))
print(head(df))

cat("\nTesting raw data...\n")
df <- gmd(variables = "rGDP", raw = TRUE)
print(head(df))

cat("\nTesting list variables...\n")
gmd(vars = TRUE)

cat("\nTesting list countries...\n")
gmd(iso = TRUE)

cat("\nTesting combined parameters...\n")
df <- gmd(
  version = "2025_01",
  country = c("USA", "CHN"),
  variables = c("rGDP", "unemp", "CPI")
)
print(head(df))

cat("\nAll README examples completed!\n")
