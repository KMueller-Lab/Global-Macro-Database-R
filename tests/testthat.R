# This file is part of the standard testthat setup.
# It ensures that tests are run when R CMD check is run.

library(testthat)
library(globalmacrodata)

test_check("globalmacrodata")
