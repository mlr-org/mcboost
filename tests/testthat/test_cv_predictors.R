test_that("TreeResidualFitters work", {
  skip_if_not_installed("rpart")
  rf = CVTreeResidualFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_is(out[[2]], "CVLearnerPredictor")
  expect_true(out[[2]]$is_fitted)
  out = out[[2]]$predict(iris[,1:4])
  expect_numeric(out)
})

test_that("MCBoost multicalibrate and predict_probs - CV Predictor", {
  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]
  set.seed(123L)
  mc = MCBoost$new(subpop_fitter = "CVTreeResidualFitter")
  mc$multicalibrate(data, labels)

  expect_list(mc$iter_models, types = "LearnerPredictor")
  expect_list(mc$iter_partitions, types = "ProbRange")

  prds = mc$predict_probs(data)
  expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))
})