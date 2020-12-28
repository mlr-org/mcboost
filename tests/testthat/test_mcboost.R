context("MCBoost Class")

test_that("MCBoost class instantiation", {
  mc = MCBoost$new(subpop_fitter = "TreeResidualFitter")
  expect_class(mc, "MCBoost")
  expect_class(mc$subpop_fitter, "ResidualFitter")
  expect_function(mc$predictor, args = "data")
})

test_that("MCBoost multicalibrate and predict_probs - ConstantPredictor", {

  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]

  mc = MCBoost$new(subpop_fitter = "TreeResidualFitter")
  mc$multicalibrate(data, labels)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  prds = mc$predict_probs(data)
  expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))
})


test_that("MCBoost multicalibrate and predict_probs - init_predictor", {

  # Sonar task
  tsk = tsk("sonar")
  d = tsk$data(cols = tsk$feature_names)
  l = tsk$data(cols = tsk$target_names)[[1]]


  prd = LearnerPredictor$new(lrn("classif.rpart"))
  prd$fit(d, l)

  init_predictor = function(data) {
    # Get response prediction from Learner
    p = prd$predict(data)
  }

  mc = MCBoost$new(init_predictor = init_predictor, subpop_fitter = "TreeResidualFitter")
  mc$multicalibrate(d, l)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  prds = mc$predict_probs(data)
  expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))


  labels_oh = one_hot(l)[,1]
  # Inir predictor accuracy:
  mean(init_predictor(d) == labels_oh)
  # MCboosted predictor accuracy
  mean(prds == labels_oh)
})
