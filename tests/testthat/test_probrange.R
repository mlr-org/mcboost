test_that("ProbRange works", {
  skip_on_cran()
  pr = ProbRange$new(0.1, 0.55)
  prs = list(
    pr2 = ProbRange$new(0.1, 0.55),
    pr3 = ProbRange$new(0, 1),
    pr4 = ProbRange$new(0,0.4)
  )

  expect_class(pr, "ProbRange")
  expect_equal(pr$lower, 0.1)
  expect_equal(pr$upper, 0.55)

  values = c(TRUE, FALSE, FALSE)
  out = mlr3misc::map_lgl(prs, function(x) {
    pr$is_equal(x)
  })
  expect_equal(out, values, check.attributes = FALSE)

  out = mlr3misc::map_lgl(prs, function(x) {
    pr$is_not_equal(x)
  })
  expect_equal(!out, values, check.attributes = FALSE)

  prs = c(0.09, 0.1, 0.4, 0.55, 0.7)
  expect_equal(pr$in_range_mask(prs), c(FALSE, TRUE, TRUE, FALSE, FALSE))

  expect_false(pr$is_equal(5))
  expect_true(pr$is_not_equal(5))

  expect_output(print(pr), "ProbRange")
})
