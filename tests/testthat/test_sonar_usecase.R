test_that("TreeAuditorFitters work", {
  skip_on_cran()
  skip_if_not_installed("rpart")
  tsk = tsk("sonar")
  data = tsk$data()[, Class := as.integer(Class) - 1L]
  mod = glm(data = data, formula = Class ~ .)
  init_predictor = function(data) {predict(mod, data)}
  d = data[, -1]
  l = data$Class
  mc = MCBoost$new(init_predictor = init_predictor)
  mc$multicalibrate(d[1:200,], l[1:200])
  expect_list(mc$iter_models, len = 5)
  out = mc$predict_probs(d[201:208,])
  expect_numeric(out)
})