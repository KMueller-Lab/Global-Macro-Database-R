devtools::load_all("C:/Users/shixu/Documents/GitHub/Global-Macro-Database-R")

df <- gmd()
df <- gmd(country = "USA")
df <- gmd(country = c("USA", "CHN", "DEU"))
df <- gmd(variables = c("rGDP", "infl", "unemp"))
df <- gmd(variables = "rGDP", country = "USA", raw = TRUE)
df <- gmd(variables = "rGDP", country = c("USA", "CHN", "DEU"), raw = TRUE)
gmd(vars = TRUE)
gmd(iso = TRUE)
df <- gmd(sources = "IMF_WEO")
gmd(sources = "list")
df <- gmd(sources = "load")
df <- gmd(sources = "IMF_WEO", variables = "nGDP",country = "USA")
df <- gmd(sources = "IMF_WEO", variables = c("rGDP", "infl", "unemp"),country = c("USA", "CHN", "DEU"))
df <- gmd(
  version = "2026_01",
  country = c("USA", "CHN"),
  variables = c("rGDP", "unemp", "CPI")
)
