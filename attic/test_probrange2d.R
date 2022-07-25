context("ProbRange2D")

test_that("ProbRange2D works", {
  pr = ProbRange$new(0.1, 0.55)
  t = ProbRange$new(40,101)
  pr2d = ProbRange2D$new(pr, t)

  prs2d = list(
    pr2 = ProbRange2D$new(ProbRange$new(0.1, 0.55),
                          ProbRange$new(40,101)),
    pr3 = ProbRange2D$new(ProbRange$new(0.3, 0.55),
                          ProbRange$new(0,100)),
    pr4 = ProbRange2D$new(ProbRange$new(0.1, 0.55),
                          ProbRange$new(0,100))
  )

  expect_class(pr2d, "ProbRange2D")
  expect_equal(pr2d$prob$lower, 0.1)
  expect_equal(pr2d$prob$upper, 0.55)
  expect_equal(pr2d$time$lower, 40)
  expect_equal(pr2d$time$upper, 101)

  values = c(TRUE, FALSE, FALSE)

  out = mlr3misc::map_lgl(prs2d, function(x) {
    pr2d$is_equal(x)
  })
  expect_equal(out, values, check.attributes = FALSE)

  out = mlr3misc::map_lgl(prs2d, function(x) {
    pr2d$is_not_equal(x)
  })
  expect_equal(!out, values, check.attributes = FALSE)

  prs = matrix(c(0.09, 0.1, 0.4, 0.55, 0.7), ncol = 5)
  time = seq(0,200,length.out = 5)
  colnames(prs) = time

  in_prob = pr2d$in_range_mask(prs)
  expect_equal(in_prob$n, TRUE)
  expect_equal(in_prob$time, c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(as.logical(in_prob$matrix), c(TRUE, TRUE))
  expect_equal(names(in_prob$matrix), c("50","100"))

  expect_false(pr2d$is_equal(5))
  expect_true(pr2d$is_not_equal(5))

  expect_output(print(pr2d), "ProbRange2D")
})
