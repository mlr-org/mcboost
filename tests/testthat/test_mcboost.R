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


test_that("MCBoost multicalibrate and predict_probs - init_predictor function", {
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

  mc = MCBoost$new(init_predictor = init_predictor, subpop_fitter = "TreeResidualFitter", eta = 0.1)
  mc$multicalibrate(d, l)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  prds = mc$predict_probs(d)
  expect_numeric(prds, lower = 0, upper = 1, len = nrow(d))


  labels_oh = one_hot(l)
  # Inir predictor accuracy:
  mean(init_predictor(d) == labels_oh)
  # MCboosted predictor accuracy
  mean(prds == labels_oh)

  expect_numeric(mc$auditor_effect(d), lower = 0, upper = 1, len = nrow(d))

})


test_that("MCBoost multicalibrate and predict_probs - Init trained LearnerPredictor - response", {
  # Sonar task
  tsk = tsk("sonar")
  data = as.matrix(tsk$data(cols = tsk$feature_names))
  labels = tsk$data(cols = tsk$target_names)[[1]]

  lp = LearnerPredictor$new(lrn("classif.rpart"))
  lp$fit(data, labels)

  mc = MCBoost$new(subpop_fitter = "TreeResidualFitter", default_model_class = lp, multiplicative = TRUE)
  mc$multicalibrate(data, labels)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  prds = mc$predict_probs(data)
  expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))
})


test_that("MCBoost multicalibrate and predict_probs - Init trained LearnerPredictor - prob", {
  # Breast Cancer task
  tsk = tsk("breast_cancer")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]

  lp = LearnerPredictor$new(lrn("classif.rpart", predict_type = "prob"))
  lp$fit(data, labels)

  mc = MCBoost$new(subpop_fitter = "TreeResidualFitter", default_model_class = lp, partition = TRUE, rebucket = TRUE)
  mc$multicalibrate(data, labels)

  expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  prds = mc$predict_probs(data)
  expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))
})


test_that("MCBoost multicalibrate with subpops", {
  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]

  # Add group indicators for subpops
  data[, AGE_LE := sample(c(0,1), nrow(data), TRUE)]
  data[, G_1 := c(rep(0, 100), rep(1, 108))]
  data[, G_2 := c(rep(1, 50), rep(0, 158))]
  subpops = list("AGE_LE", "G_1", "G_2", function(x) x[["V1"]] > 0.22)

  # Fit  initial model
  lp = LearnerPredictor$new(lrn("classif.rpart"))
  lp$fit(data, labels)

  mc = MCBoost$new(subpop_fitter = "TreeResidualFitter", default_model_class = lp,
    subpops = subpops, alpha = 0)
  mc$multicalibrate(data, labels)
  expect_list(mc$iter_models, types = "SubpopPredictor", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  assert_numeric(mc$predict_probs(data), lower = 0, upper = 1, len = nrow(data))
})


test_that("MCBoost multicalibrate with Subgroups", {
  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]

  # Fit  initial model
  lp = LearnerPredictor$new(lrn("classif.rpart"))
  lp$fit(data, labels)

  masks =  list(
    rep(c(1,0), 104),
    rep(c(1,1,1,0), 52)
  )
  sf = SubgroupFitter$new(masks)

  mc = MCBoost$new(subpop_fitter = sf, default_model_class = lp, alpha = 0, partition = FALSE)
  mc$multicalibrate(data, labels)
  expect_list(mc$iter_models, types = "SubgroupModel", len = mc$max_iter)
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

  expect_numeric(mc$predict_probs(data), lower = 0, upper = 1, len = nrow(data))
})

test_that("MCBoost various settings", {
  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  data[, "ytmp" := runif(208)]
  labels = tsk$data(cols = tsk$target_names)

  lp = LearnerPredictor$new(lrn("classif.rpart"))
  lp$fit(data, labels)

  # Check a list of settings
  mcs = list(
     MCBoost$new(subpop_fitter = NULL),
     MCBoost$new(subpop_fitter = "Foo"),
     MCBoost$new(alpha = 0.05)
  )
  for (mc in mcs) {
    mc$multicalibrate(data, labels)
    expect_list(mc$iter_models, types = "LearnerPredictor", max.len = mc$max_iter)
    expect_list(mc$iter_partitions, types = "ProbRange", max.len = mc$max_iter)
    prds = mc$predict_probs(data)
    expect_numeric(prds, lower = 0, upper = 1, len = nrow(data))
  }

  # Test for no-multicalibration setting
  mc = MCBoost$new()
  expect_warning({prd = mc$predict_probs(data)}, fixed = "multicalibrate was not run")
  expect_numeric(prd, lower = 0, upper = 1, len = nrow(data))

  # Test for large alpha setting
  mc = MCBoost$new(alpha = 1)
  mc$multicalibrate(data, labels)
  expect_warning({prd = mc$predict_probs(data)}, fixed = "multicalibrate was not run")
  expect_numeric(prd, lower = 0, upper = 1, len = nrow(data))
})


test_that("MCBoost Edge Cases", {
  # Sonar task
  tsk = tsk("sonar")
  d = tsk$data(cols = tsk$feature_names, rows = c(1:10, 200:208))
  l = tsk$data(cols = tsk$target_names, rows = c(1:10, 200:208))[[1]]

  check_predictor = function(init_predictor) {
      mc = MCBoost$new(init_predictor = init_predictor, subpop_fitter = "TreeResidualFitter", max_iter = 2L)
      mc$multicalibrate(d, l)

      expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
      expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)

      prds = mc$predict_probs(d)
      expect_numeric(prds, lower = 0, upper = 1, len = nrow(d))
      return(TRUE)
  }

  inits = list(
    function(data) rep(1L, nrow(data)),
    function(data) rep(0L, nrow(data)),
    function(data) rep(0.5, nrow(data)),
    function(data) runif(nrow(data)),
    function(data) rep(-1, nrow(data))
  )
  expect_true(all(map_lgl(inits, check_predictor)))
  expect_error(check_predictor(function(data) rep(Inf, nrow(data))))
})

test_that("MCBoost args for self-defined init predictor", {
  # Sonar task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]

  # Fit  initial model
  init_predictor = function(data, prds) {
    prds
  }
  mc = MCBoost$new(init_predictor = init_predictor)
  mc$multicalibrate(data, labels, predictor_args = runif(208))
  expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)
  expect_numeric(mc$predict_probs(data, predictor_args = runif(208)), lower = 0, upper = 1, len = nrow(data))
})

test_that("MCBoost multicalibrate and predict_probs - init_predictor function", {
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

  for (iter_strat in c("bootstrap", "split")) {
    mc = MCBoost$new(init_predictor = init_predictor, subpop_fitter = "TreeResidualFitter", iter_sampling = iter_strat, max_iter=2L)
    mc$multicalibrate(d, l)
    expect_list(mc$iter_models, types = "LearnerPredictor", len = mc$max_iter)
    expect_list(mc$iter_partitions, types = "ProbRange", len = mc$max_iter)
    prds = mc$predict_probs(d)
    expect_numeric(prds, lower = 0, upper = 1, len = nrow(d))
    labels_oh = one_hot(l)
    # Inir predictor accuracy:
    mean(init_predictor(d) == labels_oh)
    # MCboosted predictor accuracy
    mean(prds == labels_oh)
  }
})

