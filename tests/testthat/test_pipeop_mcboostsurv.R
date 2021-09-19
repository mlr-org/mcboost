context("MCBoostSurv PipeOp")

test_that("MCBoostSurv class instantiation", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if_not(require("mlr3"))
  skip_if_not(require("mlr3pipelines"))
  skip_if_not(require("mlr3proba"))
  skip_if_not(require("mlr3learners"))
  gr = gunion(list(
    "data" = po("encode") %>>% po("nop"),
    "prediction" = po("learner_pred", lrn("surv.ranger"))
  )) %>>%
    PipeOpMCBoostSurv$new(param_vals = list(multiplicative = FALSE))
  expect_is(gr, "Graph")
  tsk = tsk("rats")
  tid = sample(1:300, 200)
  train_out = gr$train(tsk$clone()$filter(tid))
  expect_is(gr$state$mcboostsurv$mc, "MCBoostSurv")
  expect_list(gr$state$mcboostsurv$mc$iter_models, types = "LearnerPredictor")
  expect_true(!gr$state$mcboostsurv$mc$multiplicative)
  pr = gr$predict(tsk$clone()$filter(setdiff(1:300, tid)))
  expect_is(pr[[1]], "Prediction")
})

test_that("MCBoostSurv ppl", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if_not(require("mlr3"))
  skip_if_not(require("mlr3pipelines"))
  
  rats = survival::rats
  rats$sex = (as.character(rats$sex))=="f"
  b = as_data_backend(rats)
  task = TaskSurv$new("rats",
                      backend = b, time = "time",
                      event = "status")
  
  
  l = lrn("surv.kaplan")$train(task)
  pp = ppl_mcboostsurv(learner = l)
  expect_is(pp, "Graph")
  pp$train(task)
  expect_true(!is.null(pp$state))
  prd = pp$predict(task)
  expect_is(prd[[1]], "PredictionSurv")
})

