context("MCBoostSurv Class")

test_that("MCBoostSurv class instantiation", {
  mc = MCBoostSurv$new(auditor_fitter = "TreeAuditorFitter")
  expect_class(mc, "MCBoostSurv")
  expect_class(mc$auditor_fitter, "AuditorFitter")
  expect_function(mc$predictor, args = "data")

})

test_that("MCBoostSurv multicalibrate and predict_probs", {
  library("mlr3learners")
  rats = survival::rats
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
                       max_iter = 3)
  mc$multicalibrate(data, labels)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange2D", len = mc$max_iter)

  prds = mc$predict_probs(data)
  expect_data_frame(prds, nrows = nrow(data), ncol = 58)
})




