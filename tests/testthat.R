if (requireNamespace("testthat", quietly = TRUE)) {
  library(checkmate)
  library(testthat)
  library(mlr3)
  library(mcboost)
  library(mlr3proba)

  test_check("mcboost")
}
