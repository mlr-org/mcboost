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
  skip_if_not_installed("rpart")
  rf = TreeResidualFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("RidgeResidualFitters work", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("glmnet")
  rf = RidgeResidualFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("SubPopFitter work", {
  data = data.table(
    "AGE_NA" = c(0, 0, 0, 0, 0),
    "AGE_0_10" =  c(1, 1, 0, 0, 0),
    "AGE_11_20" = c(0, 0, 1, 0, 0),
    "AGE_21_31" = c(0, 0, 0, 1, 1),
    "X1" = runif(5),
    "X2" = runif(5)
  )
  label = c(1,0,0,1,1)

  pops = list("AGE_NA", "AGE_0_10", "AGE_11_20", "AGE_21_31", function(x) {x[["X1" > 0.5]]})
  rf = SubpopFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0, upper = 0)
  expect_class(out[[2]], "SubpopPredictor")

  pops = list("AGE_NA")
  rf = SubpopFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0, upper = 0)
  expect_class(out[[2]], "SubpopPredictor")
})

test_that("SubGroupFitter work", {
  data = data.table(
    "AGE_0_10" =  c(1, 1, 0, 0, 0),
    "AGE_11_20" = c(0, 0, 1, 0, 0),
    "AGE_21_31" = c(0, 0, 0, 1, 1),
    "X1" = runif(5),
    "X2" = runif(5)
  )
  label = c(1,0,0,1,1)

  masks = list(
    "M1" = c(1L, 0L, 1, 1, 0),
    "M2" = c(1L, 0L, 0L, 0L,1L)
  )
  rf = SubgroupFitter$new(masks)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0, upper = 1)
  expect_class(out[[2]], "SubgroupModel")
})

test_that("SubPopFitter iterates through all columns", {
  data = data.table(
    "AGE_NA" = c(0, 0, 0, 0, 0),
    "AGE_0_10" =  c(1, 1, 0, 0, 0),
    "AGE_11_20" = c(0, 0, 1, 0, 0),
    "AGE_21_31" = c(0, 0, 0, 1, 1),
    "X1" = runif(5),
    "X2" = runif(5)
  )
  label = c(1,0,0,1,1)

  pops = list("AGE_21_31", "AGE_11_20")
  rf = SubpopFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0.2, upper = 0.2)
  expect_class(out[[2]], "SubpopPredictor")

  pops = rev(list("AGE_21_31", "AGE_11_20"))
  rf = SubpopFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0.2, upper = 0.2)
  expect_class(out[[2]], "SubpopPredictor")
})
