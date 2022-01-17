context("MCBoostSurv PipeOp")

test_that("MCBoostSurv class instantiation", {
  skip_on_cran()
  skip_on_os("solaris")
  library("mlr3pipelines")
  gr = gunion(list(
    "data" = po("encode") %>>% po("nop"),
    "prediction" = po("learner_pred", lrn("surv.ranger"))
  )) %>>%
    PipeOpMCBoostSurv$new(param_vals = list(multiplicative = FALSE, alpha = 0, max_iter = 3))
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

test_that("pipeop instantiation", {
  skip_on_cran()
  skip_on_os("solaris")
  library("mlr3pipelines")
  pop = po("mcboostsurv")
  expect_is(pop, "PipeOpMCBoostSurv")
  expect_is(pop, "PipeOp")
  expect_list(pop$param_set$values, len = 0L)
  expect_true(pop$predict_type == "distr")
})

test_that("MCBoostSurv ppl", {
  skip_on_cran()
  skip_on_os("solaris")
  library("survival")
  rats$sex = (as.character(rats$sex)) == "f"
  task = TaskSurv$new("rats",
    backend = as_data_backend(rats),
    time = "time",
    event = "status"
  )
  l = lrn("surv.kaplan")# $train(task)
  pp = ppl_mcboostsurv(learner = l, param_vals = list(max_iter = 3, alpha = 0))
  expect_is(pp, "Graph")
  pp$train(task)
  expect_true(!is.null(pp$state))
  prd = pp$predict(task)
  expect_is(prd[[1]], "PredictionSurv")
  state = pp$pipeops$mcboostsurv$state$mc
  expect_true(length(state$iter_models) == 3)
  expect_true(state$alpha == 0)
})

test_that("MCBoostSurv ppl", {
  skip_on_cran()
  gr = ppl_mcboostsurv(lrn("surv.kaplan"))
  expect_is(gr, "Graph")
})