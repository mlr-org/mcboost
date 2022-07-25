test_that("PipeOp Learner Pred", {
  skip_on_cran()
  skip_on_os("solaris")

  pop = mlr3pipelines::po("learner_pred", learner = lrn("surv.kaplan"))
  expect_is(pop, "PipeOp")

  out = pop$train(list(tsk("rats")))[[1]]
  expect_is(out, "Task")
  expect_true(all(
    c(colnames(out$data(cols = out$feature_names)) %in% 
    c("surv.kaplan.time", "surv.kaplan.status", "surv.kaplan.crank", 
      "surv.kaplan.distr"))
  ))
  dist = out$data()[["surv.kaplan.distr"]]
  expect_list(dist, types = "list", len = 300L)
  out = map(dist[[1]], function(x) expect_matrix(as.matrix(x), nrows = 300, ncols = length(tsk("rats")$unique_times())))
  
  out = pop$predict(list(tsk("rats")))[[1]]
  expect_is(out, "Task")
  expect_true(all(
    c(colnames(out$data(cols = out$feature_names)) %in%
    c("surv.kaplan.time", "surv.kaplan.status", "surv.kaplan.crank", "surv.kaplan.distr"))
  ))  
  dist = out$data()[["surv.kaplan.distr"]]
  expect_list(dist, types = "list", len = 300L)
  out = map(dist[[1]], function(x) expect_matrix(as.matrix(x), nrows = 300, ncols = length(tsk("rats")$unique_times())))
})
