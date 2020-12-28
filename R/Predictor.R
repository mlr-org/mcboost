#' Predictor
#' @export
Predictor = R6::R6Class("Predictor",
  public = list(
    #' @description
    #' Instantiate a Predictor
    initialize = function() {
      invisible(self)
    },
    #' @description
    #' Fit the predictor.
    #' @template params_data_label
    fit = function(data, labels) {
      stop("Abstract base class")
    },
    #' @description
    #' Predict a dataset with constant predictions.
    #' @param data [`data.table`] \cr
    #'   Prediction data.
    predict = function(data) {
      stop("Abstract base Class")
    }
  )
)

#' ConstantPredictor
#' @export
ConstantPredictor = R6::R6Class("ConstantPredictor",
  inherit = Predictor,
  public = list(
    #' @field constant [`numeric`]\cr
    #' mlr3 Constant to predict with.
    constant = 0.,
    #' @field is_fitted [`logical`]\cr
    #' Whether the model is fitted.
    is_fitted = TRUE,
    #' @description
    #' Instantiate a ConstantPredictor
    #'
    #' @param constant [`numeric`]\cr
    #'   Constant to predict with.
    initialize = function(constant = 0.5) {
      self$constant = assert_number(constant)
      invisible(self)
    },
    #' @description
    #' Fit the constant predictor.
    #' Does nothing.
    #' @template params_data_label
    fit = function(data, labels) {},
    #' @description
    #' Predict a dataset with constant predictions.
    #' @param data [`data.table`] \cr
    #'   Prediction data.
    predict = function(data) {
        rep(self$constant, nrow(data))
    }
  )
)

#' LearnerPredictor
#' Wraps a mlr3 Learner into a `LearnerPredictor` object that can be used
#' with mcboost.
#' @export
LearnerPredictor = R6::R6Class("LearnerPredictor",
  inherit = Predictor,
  public = list(
    #' @field learner [`Learner`]\cr
    #' mlr3 Learner used for fitting residuals.
    learner = NULL,
    #' @description
    #' Instantiate a LearnerPredictor
    #'
    #' @param learner [`Learner`]\cr
    #'   Learner used for train/predict.
    initialize = function(learner) {
      self$learner = assert_class(learner, "Learner")
    },
    #' @description
    #' Fit the learner.
    #' @template params_data_label
    fit = function(data, labels) {
      task = xy_to_task(data, labels)
      self$learner$train(task)
    },
    #' @description
    #' Predict a dataset with leaner predictions.
    #' @param data [`data.table`] \cr
    #'   Prediction data.
    predict = function(data) {
      if (inherits(self$learner, "LearnerRegr")) {
        return(self$learner$predict_newdata(data)$response)
      } else {
        prd = self$learner$predict_newdata(data)
        if ("prob" %in% self$learner$predict_type) {
          p = prd$prob
        } else {
          p = one_hot(prd$response)
        }
        if (ncol(p) == 2L) {
          p = p[,1]
        }
        return(p)
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