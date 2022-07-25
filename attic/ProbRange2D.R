#' Range of Probabilities and Time
#' @description
#' Range of format list(prob = [lower_prob; upper_prob), time = [lower_time; upper_time).
#' @noRd
ProbRange2D = R6::R6Class("ProbRange2D",
  public = list(
    #' @field prob [`ProbRange`] \cr
    #'   Range of probabilities
    prob = NULL,
    #' @field time [`ProbRange`] \cr
    #'   Range of time
    time = NULL,
    #' @field aggregation [`function`] \cr
    #' Type of aggregation, if there are several values per row
    #' (e.g., mean or median)
    aggregation = NULL,


    #' @description
    #' Instantiate a Probability Range 2D
    #'
    #' @param prob [`ProbRange`]\cr
    #'   Range of probabilities
    #' @param time [`ProbRange`]\cr
    #'   Range of time
    #' @param aggregation [`character`]\cr
    #'   Aggegation
    # FIXME
    #' @return [`ProbRange2D`]
    initialize = function(prob = ProbRange$new(), time = ProbRange$new(), aggregation = NULL) {
      self$prob = assert_r6(prob, "ProbRange")
      self$time = assert_r6(time, "ProbRange")

      self$aggregation = assert_function(aggregation, null.ok = TRUE)
      invisible(self)
    },
    #' @description
    #' Compare with 'other' Probability Range (2D) regarding equality
    #'
    #' @param other [`ProbRange2D`]\cr
    #'   ProbRange2D to compare to.
    #' @return
    #' Logical, whether ProbRanges2D are equal.
    is_equal = function(other) {
      if (test_class(other, "ProbRange2D")) {
        return(self$prob$is_equal(other$prob) && self$time$is_equal(other$time))
      }
      return(FALSE)
    },
    #' @description
    #' Compare with 'other' ProbabRange2D regarding in-equality
    #'
    #' @param other [`ProbRange2D`]\cr
    #'   ProbRange2D to compare to.
    #' @return
    #' Logical, whether ProbRanges2D are in-equal.
    is_not_equal = function(other) {
      if (test_class(other, "ProbRange2D")) {
        return(self$prob$is_not_equal(other$prob) || self$time$is_not_equal(other$time))
      }
      return(TRUE)
    },
    #' @description
    #' Check whether elements of an array with dimensions individuals x time_points
    #' are in the ProbRange2D.
    #'
    #' @param x [`numeric`]\cr
    #'   Matrix of probabilities with times as columnnames
    #' @return
    #'   Logical array, whether elements are in ProbRange2D or not.
    in_range_mask = function(x) {

      time = as.numeric(colnames(x))

      time_mask = self$time$in_range_mask(time)

      if (!(any(time_mask))) {
        return(NULL)
      }


      # do we take the mean to decide in which bucket? (= one bucket per subject)
      if (!is.null(self$aggregation)) {
        x = x[, time_mask]

        if (length(dim(x)) < 1) {
          return(NULL)
        }

        x = apply(x, 1, self$aggregation)
        n = self$prob$in_range_mask(x)

        if (!(any(n))) {
          return(NULL)
        }

        matrix = matrix(TRUE, nrow = sum(n), ncol = sum(time_mask))
      } else {
        matrix = self$prob$in_range_mask(x)
        matrix [, !time_mask] = FALSE
        n = apply(matrix, 1, any)

        if (!(any(n))) {
          return(NULL)
        }

        matrix = matrix[n, time_mask]
      }
      return(list(
        matrix = matrix,
        n = n,
        time = time_mask))
    },
    #' @description
    #' Printer for ProbRange2D
    print = function() {
      cat(paste0("ProbRange2D:  Probability Range: [", self$prob$lower, ";", self$prob$upper, "),
                 Time Range[", self$time$lower, ";", self$time$upper, ")"))
    }
  )
)
