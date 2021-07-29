context("AuditorFitters")

test_that("AuditorFitters work", {
  rf = AuditorFitter$new()
  expect_class(rf, "AuditorFitter")
  expect_error(rf$fit(1, 1), "Not implemented")
})


test_that("LearnerAuditorFitters work", {
  skip_on_cran()
  rf = LearnerAuditorFitter$new(lrn("regr.featureless"))
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("TreeAuditorFitters work", {
  skip_if_not_installed("rpart")
  rf = TreeAuditorFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("RidgeAuditorFitters work", {
  skip_on_cran()
  skip_on_os("mac")
  skip_on_os("solaris")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("glmnet")

  rf = RidgeAuditorFitter$new()
  out = rf$fit(iris[, 1:4], runif(150))
  expect_number(out[[1]])
  expect_class(out[[2]], "LearnerPredictor")
  expect_true(out[[2]]$is_fitted)
})

test_that("SubPopFitter work", {
  skip_on_cran()
  data = data.table(
    "AGE_NA" = c(0, 0, 0, 0, 0),
    "AGE_0_10" =  c(1, 1, 0, 0, 0),
    "AGE_11_20" = c(0, 0, 1, 0, 0),
    "AGE_21_31" = c(0, 0, 0, 1, 1),
    "X1" = runif(5),
    "X2" = runif(5)
  )
  label = c(1,0,0,1,1)

  pops = list("AGE_NA", "AGE_0_10", "AGE_11_20", "AGE_21_31", function(x) {x[["X1" > 0.5]]})
  rf = SubpopAuditorFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0.2, upper = 0.2)
  expect_class(out[[2]], "SubpopPredictor")

  pops = list("AGE_NA")
  rf = SubpopAuditorFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0, upper = 0)
  expect_class(out[[2]], "SubpopPredictor")
})

test_that("SubGroupFitter work", {
  skip_on_cran()
  data = data.table(
    "AGE_0_10" =  c(1, 1, 0, 0, 0),
    "AGE_11_20" = c(0, 0, 1, 0, 0),
    "AGE_21_31" = c(0, 0, 0, 1, 1),
    "X1" = runif(5),
    "X2" = runif(5)
  )
  label = c(1,0,0,1,1)

  masks = list(
    "M1" = c(1L, 0L, 1L, 1L, 0L),
    "M2" = c(1L, 0L, 0L, 0L, 1L)
  )
  rf = SubgroupAuditorFitter$new(masks)
  out = rf$fit(data, label - 0.5, rep(1, length(label)))
  expect_list(out)
  expect_number(out[[1]], lower = 0, upper = 1)
  expect_class(out[[2]], "SubgroupModel")
})

test_that("SubPopFitter iterates through all columns", {
  skip_on_cran()
  data = data.table(
    "AGE_NA" = c(0, 0, 0, 0, 0),
    "AGE_0_10" =  c(1, 1, 0, 0, 0),
    "AGE_11_20" = c(0, 0, 1, 0, 0),
    "AGE_21_31" = c(0, 0, 0, 1, 1),
    "X1" = runif(5),
    "X2" = runif(5)
  )
  label = c(1,0,0,1,1)

  pops = list("AGE_21_31", "AGE_11_20")
  rf = SubpopAuditorFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0.2, upper = 0.2)
  expect_class(out[[2]], "SubpopPredictor")

  pops = rev(list("AGE_21_31", "AGE_11_20"))
  rf = SubpopAuditorFitter$new(subpops = pops)
  out = rf$fit(data, label - 0.5)
  expect_list(out)
  expect_number(out[[1]], lower = 0.2, upper = 0.2)
  expect_class(out[[2]], "SubpopPredictor")
})

test_that("SubPopFitter throws proper error if not binary or wrong length", {
  skip_on_cran()
  data = data.frame(X1 = rnorm(n = 10L), X2 = rnorm(n = 10L))
  rs = c(1, rep(0, 9))

  # 0/1 chracters are fine
  masks =  list(
    rep(c("1", "0"), 5)
  )
  sf = SubgroupAuditorFitter$new(masks)

  mean1 = sf$fit(data = data, resid = rs,rep(1, length(rs)))
  sm =  SubgroupModel$new(masks)
  mean2 = sm$fit(data = data, labels = rs) # should not be 1!

  # wrong type
  masks =  list(
    c("ab", "cc")
  )
  expect_error(SubgroupAuditorFitter$new(masks), "subgroup_masks must be a list of integers")

  # wrong length
  masks = list(
    rep(c(1, 0), 10)
  )
  sf = SubgroupAuditorFitter$new(masks)
  expect_error(sf$fit(data = data, resid = rs, mask = rep(1, 20)), "Length of subgroup masks must match length of data")

  # not binary
  masks = list(
    rep(c(1, 3, 0, 4))
  )
  expect_error(SubgroupAuditorFitter$new(masks), "subgroup_masks must be binary vectors")
})

test_that("Bug in SubgroupAuditorFitter #16", {
  skip_on_cran()
  data = data.frame(X1 = rnorm(n = 10L), X2 = rnorm(n = 10L))
  masks =  list(
      rep(c(1, 0), 5)
   )
  sf = SubgroupAuditorFitter$new(masks)
  resid = c(1, rep(0, 9))
  sm =  SubgroupModel$new(masks)
  mn = sm$fit(data = data, labels = resid)
  expect_equal(mn[[1]],  mean(masks[[1]] * resid) / mean(masks[[1]]))
})
