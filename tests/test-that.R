library(testthat)
Sys.setenv("R_TESTS" = "") ##See https://github.com/hadley/testthat/issues/86
test_check("rainfallABC")
