# The Global Macro Database (R Package)
<a href="https://www.globalmacrodata.com" target="_blank" rel="noopener noreferrer">
    <img src="https://img.shields.io/badge/Website-Visit-blue?style=flat&logo=google-chrome" alt="Website Badge">
</a>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

[Link to paper 📄](https://www.globalmacrodata.com/research-paper.html)

This repository complements paper, **Müller, Xu, Lehbib, and Chen (2025)**, which introduces a panel dataset of **46 macroeconomic variables across 243 countries** from historical records beginning in the year **1086** until **2024**, including projections through the year **2030**.

## Features

- **Unparalleled Coverage**: Combines data from **32 contemporary sources** (e.g., IMF, World Bank, OECD) with **78 historical datasets**.
- **Extensive Variables**: GDP, inflation, government finance, trade, employment, interest rates, and more.
- **Harmonized Data**: Resolves inconsistencies and splices all available data together.
- **Scheduled Updates**: Regular releases ensure data reliability.
- **Full Transparency**: All code is open source and available in this repository.
- **Accessible Formats**: Provided in `.dta`, `.csv` and as **<a href="https://github.com/KMueller-Lab/Global-Macro-Database" target="_blank" rel="noopener noreferrer">Stata</a>
/<a href="https://github.com/KMueller-Lab/Global-Macro-Database-Python" target="_blank" rel="noopener noreferrer">Python</a>/<a href="https://github.com/KMueller-Lab/Global-Macro-Database-R" target="_blank" rel="noopener noreferrer">R</a> package**.

## Data access

<a href="https://www.globalmacrodata.com/data.html" target="_blank" rel="noopener noreferrer">Download via website</a>

**R package:**
```R
# Install from GitHub
devtools::install_github("KMueller-Lab/Global-Macro-Database-R")
```

**How to use (examples)**
```R
library(globalmacrodata)

# Get data from latest available version
df <- gmd()

# Get data from a specific version
df <- gmd(version = "2025_01")

# Get data for a specific country
df <- gmd(country = "USA")

# Get data for multiple countries
df <- gmd(country = c("USA", "CHN", "DEU"))

# Get specific variables
df <- gmd(variables = c("rGDP", "infl", "unemp"))

# Get raw data for a single variable
df <- gmd(variables = "rGDP", raw = TRUE)

# List available variables and their descriptions
gmd(vars = TRUE)

# Get available variables and their descriptions
df <- gmd(vars = TRUE)

# List available countries and their ISO codes
gmd(iso = TRUE)

# Get available countries and their ISO codes
df <- gmd(iso = TRUE)

# Combine parameters
df <- gmd(
  version = "2025_01",
  country = c("USA", "CHN"),
  variables = c("rGDP", "unemp", "CPI")
)
```

## Parameters
- **variables (character or vector)**: Variable code(s) to include (e.g., "rGDP" or c("rGDP", "unemp"))
- **country (character or vector)**: ISO3 country code(s) (e.g., "SGP" or c("MRT", "SGP"))
- **version (character)**: Dataset version in format 'YYYY_MM' (e.g., '2025_01'). If NULL or "current", uses the latest version
- **raw (logical)**: If TRUE, download raw data for a single variable
- **iso (logical)**: If TRUE, display list of available countries
- **vars (logical)**: If TRUE, display list of available variables

## Release schedule 
| Release Date | Details         |
|--------------|-----------------|
| 2025-01-30   | Initial release: 2025_01 |
| 2025-04-01   | 2025_03         |
| 2025-07-01   | 2025_06         |
| 2025-10-01   | 2025_09         |
| 2026-01-01   | 2025_12         |

## Citation

To cite this dataset, please use the following reference:

```bibtex
@techreport{mueller2025global, 
    title = {The Global Macro Database: A New International Macroeconomic Dataset}, 
    author = {Müller, Karsten and Xu, Chenzi and Lehbib, Mohamed and Chen, Ziliang}, 
    year = {2025}, 
    type = {Working Paper}
}
```

## Acknowledgments

The development of the Global Macro Database would not have been possible without the generous funding provided by the Singapore Ministry of Education (MOE) through the PYP grants (WBS A-0003319-01-00 and A-0003319-02-00), a Tier 1 grant (A-8001749- 00-00), and the NUS Risk Management Institute (A-8002360-00-00). This financial support laid the foundation for the successful completion of this extensive project.
