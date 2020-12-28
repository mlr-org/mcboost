if (requireNamespace("testthat", quietly = TRUE)) {
  library(checkmate)
  library(testthat)
  library(mlr3)
  library(mcboost)

  test_check("mcboost")
}