skip_if_not_installed("mlr3")

test_that("MCBoost multicalibrate with subpops d = 2", {
  skip_on_os("solaris")
  skip_on_cran()
  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]

  # Add group indicators for subpops
  data[, AGE_LE := sample(c(0,1), nrow(data), TRUE)]
  data[, G_1 := c(rep(0, 100), rep(1, 108))]
  data[, G_2 := c(rep(1, 50), rep(0, 158))]
  subpops = list("AGE_LE", "G_1", "G_2", function(x) x[["V1"]] > quantile(data$V1,.9))

  # Fit  initial model
  lp = LearnerPredictor$new(lrn("classif.rpart", maxdepth = 1L, predict_type = "prob"))
  lp$fit(data, labels)

  mc = MCBoost$new(default_model_class = lp, subpops = subpops, alpha = 0, weight_degree = 2L)
  mc$multicalibrate(data, labels)
  expect_is(mc$auditor_fitter, "SubpopAuditorFitter")
  expect_list(mc$iter_models, types = "SubpopPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)
  expect_numeric(mc$predict_probs(data), lower = 0, upper = 1, len = nrow(data))
})
