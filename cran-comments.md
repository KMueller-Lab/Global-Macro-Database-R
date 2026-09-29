## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes

This is the first CRAN submission of the Global Macro Database R package (GMD R).

### Changes in v1.2.0

- Fixed: moved `haven` from Suggests to Imports (required for reading .dta files)
- Aligned: version number with Python and MATLAB implementations (v1.2.0)

### Installation tested

- Package installs correctly with dependencies
- `gmd(country = c("USA", "CHN"), variables = c("rGDP", "infl"))` returns expected data
- All unit tests pass

### Package information

- **Package**: Global Macro Database R Package (GMD R)
- **Authors**: Mohamed Lehbib, Karsten Müller
- **License**: MIT
- **Repository**: https://github.com/KMueller-Lab/Global-Macro-Database-R
- **Paper**: Müller, K., Xu, C., Lehbib, M., & Chen, Z. (2025). The Global Macro Database: A New International Macroeconomic Dataset (NBER Working Paper No. 33714).

### Data source

The package fetches macroeconomic data from a public S3 bucket and GitHub mirror. No local data files included in package — all data downloaded on demand.
