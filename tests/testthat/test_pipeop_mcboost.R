context("MCBoost PipeOp")

test_that("MCBoost class instantiation", {
  skip_if_not(require("mlr3"))
  skip_if_not(require("mlr3pipelines"))
  gr = gunion(list(
    "data" = po("nop"),
    "prediction" = po("learner_cv", lrn("classif.rpart"))
    )) %>>%
    PipeOpMCBoost$new(param_vals = list(multiplicative = FALSE))
  expect_is(gr, "Graph")
  tsk = tsk("sonar")
  tid = sample(1:208, 108)
  train_out = gr$train(tsk$clone()$filter(tid))
  expect_is(gr$state$mcboost$mc, "MCBoost")
  expect_list(gr$state$mcboost$mc$iter_models, types = "LearnerPredictor")
  expect_true(!gr$state$mcboost$mc$multiplicative)
  pr = gr$predict(tsk$clone()$filter(setdiff(1:208, tid)))
  expect_is(pr[[1]], "Prediction")
})

test_that("MCBoost ppl", {
  skip_if_not(require("mlr3"))
  skip_if_not(require("mlr3pipelines"))

  l = lrn("classif.featureless")$train(tsk("sonar"))
  pp = ppl_mcboost()
  expect_is(pp, "Graph")
  pp$param_set$values$mcboost.init_predictor = l
  pp$train(tsk("sonar"))
  expect_true(!is.null(pp$state))
  prd = pp$predict(tsk("sonar"))
  expect_is(prd[[1]], "PredictionClassif")
})
