context("ResidualFitters")


test_that("ResidualFitters work", {
  rf = ResidualFitter$new()
  expect_class(rf, "ResidualFitter")
  expect_error(rf$fit(1, 1), "Not implemented")
})


test_that("LearnerResidualFitters work", {
  rf = LearnerResidualFitter$new(lrn("regr.featureless"))
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("TreeResidualFitters work", {
  rf = TreeResidualFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("RidgeResidualFitters work", {
  rf = RidgeResidualFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})