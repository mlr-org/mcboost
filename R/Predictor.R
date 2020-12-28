#' ConstantPredictor
#' @export
ConstantPredictor = R6::R6Class("ConstantPredictor",
  public = list(
    constant = 0.,
    is_fitted = TRUE,
    initialize = function(constant = 0.5) {
      self$constant = assert_number(constant)
    },

    fit = function(data, labels) {
    },
    predict = function(data) {
        rep(self$constant, nrow(data))
    }
  )
)


#' LearnerPredictor
#' @export
LearnerPredictor = R6::R6Class("LearnerPredictor",
  public = list(
    #' @field learner [`Learner`]\cr
    #' mlr3 Learner used for fitting residuals.
    learner = NULL,

    initialize = function(learner) {
      self$learner = assert_class(learner, "Learner")
    },
    fit = function(data, labels) {
      task = xy_to_task(data, labels)
      self$learner$train(task)
    },
    predict = function(data) {
      if (inherits(self$learner, "LearnerRegr")) {
        self$learner$predict_newdata(data)$response
      } else {
        self$learner$predict_newdata(data)
      }
    }
  ),
  active = list(
    #' @field is_fitted [`logical`]\cr
    #' Whether the Learner is trained
    is_fitted = function() {
      !is.null(self$learner$state)
    }
  )
)