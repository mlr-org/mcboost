#' Range of Probabilities
#' @description
#' Range of format [lower; upper).
#' @noRd
ProbRange = R6::R6Class("ProbRange",
  public = list(
    #' @field lower [`numeric`] \cr
    #'   Lower bound of the ProbRange.
    lower = -Inf,

    #' @field upper [`numeric`] \cr
    #'   upper bound of the ProbRange.
    upper = Inf,
    #' @description
    #' Instantiate a Probability Range
    #'
    #' @param lower [`numeric`]\cr
    #'   Lower bound of the ProbRange.
    #' @param upper [`numeric`]\cr
    #'   Upper bound of the ProbRange.
    #' @return [`ProbRange`]
    initialize = function(lower = -Inf, upper = Inf) {
      self$lower = assert_number(lower)
      self$upper = assert_number(upper)
      invisible(self)
    },
    #' @description
    #' Compare with 'other' Probability Range regarding equality
    #'
    #' @param other [`ProbRange`]\cr
    #'   ProbRange to compare to.
    #' @return
    #' Logical, whether ProbRanges are equal.
    is_equal = function(other) {
      if (test_class(other, "ProbRange"))
        return((self$lower == other$lower) && (self$upper == other$upper))
      return(FALSE)
    },
    #' @description
    #' Compare with 'other' Probability Range regarding in-equality
    #'
    #' @param other [`ProbRange`]\cr
    #'   ProbRange to compare to.
    #' @return
    #' Logical, whether ProbRanges are in-equal.
    is_not_equal = function(other) {
      if (test_class(other, "ProbRange"))
        return(!((self$lower == other$lower) && (self$upper == other$upper)))
      return(TRUE)
    },
    #' @description
    #' Check whether elements of an array are in the ProbRange.
    #'
    #' @param x [`numeric`]\cr
    #'   Array of probabilities
    #' @return
    #'   Logical array, whether elements are in ProbRange or not.
    in_range_mask = function(x) {
      (x >= self$lower) & (x < self$upper)
    },
    #' @description
    #' Printer for ProbRange
    print = function() {
      cat(paste0("ProbRange: [", self$lower, ";", self$upper, ")\n"))
    }
  )
)
