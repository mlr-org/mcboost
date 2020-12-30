context("Predictor")

test_that("Predictor class instantiation", {
  prd = Predictor$new()
  expect_class(prd, "Predictor")
  expect_error(prd$fit(), fixed = "Abstract base class")
  expect_error(prd$predict(), fixed = "Abstract base class")
})

test_that("ConstantPredictor", {
  prd = ConstantPredictor$new(0.7)
  expect_class(prd, "ConstantPredictor")
  assert_true(prd$is_fitted)
  p = prd$predict(data.frame(x = 1:3))
  expect_equal(p, rep(0.7, 3))
})

test_that("LearnerPredictor - response", {
  prd = LearnerPredictor$new(lrn("classif.rpart"))
  expect_class(prd, "LearnerPredictor")
  prd$fit(iris[1:100,1:4], factor(iris$Species[1:100]))
  assert_true(prd$is_fitted)
  p = prd$predict(iris[,1:4])
  assert_numeric(p, len = 150L, lower = 0, upper = 1)
})

test_that("LearnerPredictor - probs", {
  prd = LearnerPredictor$new(lrn("classif.rpart", predict_type = "prob"))
  expect_class(prd, "LearnerPredictor")
  prd$fit(iris[51:150,1:4], factor(iris$Species[51:150]))
  assert_true(prd$is_fitted)
  p = prd$predict(iris[,1:4])
  assert_numeric(p, len = 150L, lower = 0, upper = 1)
})
