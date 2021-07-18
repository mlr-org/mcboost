#' Range of Probabilities
#' @description
#' Range of format list(individuals = [lower; upper), time_points = [lower_time; upper_time).
#' @noRd
ProbRange2D = R6::R6Class("ProbRange2D",
  public = list(
    #' @field lower [`numeric`] \cr
    #'   Lower bound of the ProbRange2D.
    lower = -Inf,
    
    #' @field upper [`numeric`] \cr
    #'   upper bound of the ProbRange2D.
    upper = Inf,
    #' @field lower_time [`numeric`] \cr
    #'   Lower bound of the ProbRange2D.
    lower_time = -Inf,
    
    #' @field upper_time [`numeric`] \cr
    #'   upper bound of the ProbRange2D.
    upper_time = Inf,
    
    #' @field aggregation [`fuction`] \cr
    #' Type of aggregation, if there are several values per row 
    #' (e.g., mean or median)
    aggregation = NULL,

    
    #' @description
    #' Instantiate a Probability Range 2D
    #'
    #' @param lower [`numeric`]\cr
    #'   Lower bound of the ProbRange2D (individuals).
    #' @param upper [`numeric`]\cr
    #'   Upper bound of the ProbRange2D (individuals).
    #' @param lower_time [`numeric`]\cr
    #'   Lower bound of the ProbRange2D (time_points).
    #' @param upper_time [`numeric`]\cr
    #'   Upper bound of the ProbRange2D (time_points).
    #' @param aggregation [`character`]\cr
    #'   Upper bound of the ProbRange2D (time_points).
    #' @return [`ProbRange2D`]
    initialize = function(lower = -Inf, upper = Inf, lower_time = -Inf, 
                          upper_time = Inf, aggregation = mean) {
      self$lower = assert_number(lower)
      self$upper = assert_number(upper)
      self$lower_time = assert_number(lower_time)
      self$upper_time = assert_number(upper_time)
      self$aggregation = assert_function(aggregation)
      invisible(self)
    },
    #' @description
    #' Compare with 'other' Probability Range regarding equality
    #'
    #' @param other [`ProbRange2D`]\cr
    #'   ProbRange2D to compare to.
    #' @return
    #' Logical, whether ProbRanges2D are equal.
    is_equal = function(other) {
      if (test_class(other, "ProbRange2D"))
        return((self$lower == other$lower) 
               && (self$upper == other$upper)
               && (self$lower_time == other$lower_time) 
               && (self$upper_time == other$upper_time))
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
      if (test_class(other, "ProbRange2D"))
        return(!((self$lower == other$lower) &&
                  (self$upper == other$upper) &&
                  (self$lower_time == other$lower_time) &&
                  (self$upper_time == other$upper_time)))
      return(TRUE)
    },
    #' @description
    #' Check whether elements of an array with dimensions individuals x time_points 
    #' are in the ProbRange2D.
    #'
    #' @param x [`numeric`]\cr
    #'   Array of probabilities
    #' @return
    #'   Logical array, whether elements are in ProbRange2D or not.
    in_range_mask = function(x) {
      #FIXME Was ist wenn nur eine Spalte? 
      #if(is.vector(x))
        #return((x >= self$lower) & (x < self$upper))
      
      #aggregate values per row
      x_agg = apply(x, MARGIN = 1, FUN = self$aggregation)
      mask = (x_agg >= self$lower) & (x_agg < self$upper)
      
      
      time_points = assert_numeric(as.numeric(colnames(x)), lower = 0, 
                                   any.missing = FALSE, min.len = 1, 
                                   unique = TRUE, null.ok = FALSE) 
      
      mask_time = ((time_points >= self$lower_time) 
                   & (time_points < self$upper_time))
      
      return(list(mask = mask, mask_time = mask_time))
      
    },
    #' @description
    #' Printer for ProbRange2D
    print = function() {
      cat(paste0("ProbRange2D:  Probability Range: [", self$lower, ";", self$upper, "), 
                 Time Range[", self$lower_time, ";", self$upper_time,")"))
    }
  )
)
