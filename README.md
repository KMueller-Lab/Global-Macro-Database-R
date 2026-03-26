# The Global Macro Database (R Package)
<a href="https://www.globalmacrodata.com" target="_blank" rel="noopener noreferrer">
    <img src="https://img.shields.io/badge/Website-Visit-blue?style=flat&logo=google-chrome" alt="Website Badge">
</a>

[![License: Non-Commercial](https://img.shields.io/badge/License-Non--Commercial-red.svg)](LICENSE)

[Link to paper](https://www.globalmacrodata.com/research-paper.html)

This repository complements the paper, **Müller, Xu, Lehbib, and Chen (2025)**, which introduces a panel dataset of **74 macroeconomic variables across 243 countries** from historical records beginning in the year **1086** until **2024**, including projections through the year **2030**.

## Features

- **Unparalleled Coverage**: Combines data from more than **100 contemporary and historical sources** (e.g., IMF, World Bank, OECD).
- **Extensive Variables**: GDP, inflation, government finance, trade, employment, interest rates, and more.
- **Harmonized Data**: Resolves inconsistencies and splices all available data together.
- **Scheduled Updates**: Regular releases ensure data reliability.
- **Full Transparency**: All code is open source and available in this repository.
- **Accessible Formats**: Provided in `.dta`, `.csv` and as **<a href="https://github.com/KMueller-Lab/Global-Macro-Database-Stata" target="_blank" rel="noopener noreferrer">Stata</a>
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

# List all available versions
gmd(version = "list")

# Get data for a specific country
df <- gmd(country = "USA")

# Get data for multiple countries
df <- gmd(country = c("USA", "CHN", "DEU"))

# Get specific variables
df <- gmd(variables = c("rGDP", "infl", "unemp"))

# Get raw data for a single variable
df <- gmd(variables = "rGDP", raw = TRUE)

# List available variables and their descriptions
df <- gmd(vars = TRUE)

# List available countries and their ISO codes
df <- gmd(iso = TRUE)

# Access data from a specific source (e.g., IMF World Economic Outlook)
df <- gmd(sources = "IMF_WEO")

# Access specific variables from a source
df <- gmd(sources = "IMF_WEO", variables = "nGDP")

# List all available sources
gmd(sources = "list")

# Load the full source list as a dataframe
df <- gmd(sources = "load")

# Get BibTeX citation for a specific source
gmd(cite = "GMD")

# Load the full citation list as a dataframe
df <- gmd(cite = "load")

# Combine parameters
df <- gmd(
  version = "2025_01",
  country = c("USA", "CHN"),
  variables = c("rGDP", "unemp", "CPI")
)
```

## Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| **variables** | character or vector | Variable code(s) to include (e.g., `"rGDP"` or `c("rGDP", "unemp")`) |
| **country** | character or vector | ISO3 country code(s) (e.g., `"SGP"` or `c("MRT", "SGP")`) |
| **version** | character | Dataset version in format `"YYYY_MM"` (e.g., `"2025_01"`). Use `"current"` for the latest version, `"list"` to see all available versions |
| **raw** | logical | If `TRUE`, download raw data for a single variable |
| **iso** | logical | If `TRUE`, display list of available countries |
| **vars** | logical | If `TRUE`, display list of available variables |
| **sources** | character | `"load"` to load source list, `"list"` to print sources, or a source name (e.g., `"IMF_IFS"`) to load that source's data. You can combine with `variables` to load only specific variables from that source |
| **cite** | character | `"load"` to load citation list, or a source key (e.g., `"GMD"`) to display its BibTeX |

## Release schedule
| Release Date | Details         |
|--------------|-----------------|
| 2025-01-30   | Initial release: 2025_01 |
| 2025-04-01   | 2025_03         |
| 2025-07-01   | 2025_06         |
| 2025-10-01   | 2025_09         |
| 2026-01-01   | 2025_12         |

## Citation

When using the Global Macro Database, please cite the following NBER Working Paper:

**Müller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database: A New International Macroeconomic Dataset (NBER Working Paper No. 33714).**

```bibtex
@techreport{mueller2025global,
    title = {{The Global Macro Database: A New International Macroeconomic Dataset}},
    author = {Müller, Karsten and Xu, Chenzi and Lehbib, Mohamed and Chen, Ziliang},
    institution = {National Bureau of Economic Research},
    type = "Working Paper",
    series = "Working Paper Series",
    number = "33714",
    year = "2025",
    month = "April",
    doi = {10.3386/w33714},
    URL = "http://www.nber.org/papers/w33714",
}
```

## Documentation

You can find the [Technical Appendix](https://gmd-releases.s3.ap-southeast-2.amazonaws.com/data/distribute/GMD_TA.pdf) on the official [website](https://www.globalmacrodata.com).

Please visit this [repository](https://github.com/KMueller-Lab/Global-Macro-Database) to access the project source code.

## Authors

*   **Mohamed Lehbib** (National University of Singapore) - [lehbib@u.nus.edu](mailto:lehbib@u.nus.edu)
*   **Karsten Müller** (National University of Singapore) - [kmueller@nus.edu.sg](mailto:kmueller@nus.edu.sg) - [Website](https://www.karstenmueller.com)

## License & Terms of Use

The data is available for **non-commercial use only**. By using this package, you agree to the terms of use outlined on the [GMD website](https://www.globalmacrodata.com).

For license enquiries, please email [kmueller@globalmacrodata.com](mailto:kmueller@globalmacrodata.com).

## Acknowledgments

The development of the Global Macro Database would not have been possible without the generous funding provided by the Singapore Ministry of Education (MOE) through the PYP grants (WBS A-0003319-01-00 and A-0003319-02-00), a Tier 1 grant (A-8001749- 00-00), and the NUS Risk Management Institute (A-8002360-00-00). This financial support laid the foundation for the successful completion of this extensive project.
