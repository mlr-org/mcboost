context("MCBoostSurv Class")

test_that("MCBoostSurv class instantiation", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if_not_installed("mlr3proba")
  mc = MCBoostSurv$new(auditor_fitter = "TreeAuditorFitter")
  expect_class(mc, "MCBoostSurv")
  expect_class(mc$auditor_fitter, "AuditorFitter")
  expect_function(mc$predictor, args = "data")
})

test_that("MCBoostSurv multicalibrate and predict_probs", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("survival")
  library("mlr3learners")
  library("mlr3proba")
  library("survival")
  rats$sex = (as.character(rats$sex))=="f"
  b = as_data_backend(rats)
  tsk = TaskSurv$new("rats",
                      backend = b, time = "time",
                      event = "status")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)
  l = lrn("surv.ranger")$train(tsk)

  mc = MCBoostSurv$new(init_predictor = l,
                       auditor_fitter = "TreeAuditorFitter",
                       max_iter = 3, 
                       alpha = 0)
  mc$multicalibrate(data, labels)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange2D", len = mc$max_iter)

  prds = mc$predict_probs(data)
  expect_data_frame(prds, nrows = nrow(data), ncol = length(tsk$unique_times()))


  mc = MCBoostSurv$new(
    init_predictor = l,
    auditor_fitter = "TreeAuditorFitter",
    max_iter = 3, 
    alpha = 0.001,
    loss = "brier",
    multiplicative = FALSE,
    max_time_quantile = 0.9
  )
  mc$multicalibrate(data, labels)
  ae = mc$auditor_effect(data)
  expect_matrix(ae, nrows = 300, ncols = length(tsk$unique_times()))
  expect_true(mc$loss == "brier")
})
