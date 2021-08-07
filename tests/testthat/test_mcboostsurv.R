context("MCBoostSurv Class")

test_that("MCBoostSurv class instantiation", {
  mc = MCBoostSurv$new(auditor_fitter = "TreeAuditorFitter")
  expect_class(mc, "MCBoostSurv")
  expect_class(mc$auditor_fitter, "AuditorFitter")
  expect_function(mc$predictor, args = "data")

})

#FIXME


# test_that("MCBoostSurv multicalibrate and predict_probs - ConstantPredictor", {
#   # Sonar task
#   tsk = tsk("rats")
#   data = tsk$data(cols = tsk$feature_names)
#   labels = tsk$data(cols = tsk$target_names)
#
#   mc = MCBoostSurv$new(auditor_fitter = "TreeAuditorFitter")
#   mc$multicalibrate(data, labels)
#
#   expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
#   expect_list(mc$iter_partitions, types = "ProbRange2D", len = mc$max_iter)
#
#   prds = mc$predict_probs(data)
#   expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))
# })
#
#
