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
    #' @param ... [`any`] \cr
    #'   Not used, only for compatibility with other methods.
    predict = function(data, ...) {
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
    #' @param ... [`any`] \cr
    #'   Not used, only for compatibility with other methods.
    predict = function(data, ...) {
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
    #' @param ... [`any`] \cr
    #'   Not used, only for compatibility with other methods.
    predict = function(data, ...) {
      if (inherits(self$learner, "LearnerRegr")) {
        return(self$learner$predict_newdata(data)$response)
      } else {
        prd = self$learner$predict_newdata(data)
        if ("prob" %in% self$learner$predict_type) {
          p = prd$prob
          if (ncol(p) == 2L) p = p[,1L]
        } else {
          p = one_hot(prd$response)
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


#' SubpopPredictor
#' @export
SubpopPredictor = R6::R6Class("SubpopPredictor",
  inherit = Predictor,
  public = list(

    #' @field subpop [`function`] \cr
    #'   A [`function`] that evaluates to binary for each row in a dataset.
    #'   Defines a sub-population.
    subpop = NULL,
    #' @field value [`numeric`] \cr
    #'   A correlation value.
    value = numeric(1),

    #' @description
    #' Instantiate a SubpopPredictor
    #' @param subpop [`character`]|[`function`] \cr
    #'   Either a [`function`], that yields a binary value for each
    #'   row in a dataset, or a [`character`] string referring to a
    #'   feature column, that defines a sub-population.
    #' @param value [`numeric`] \cr
    #'   Correlation value for the given subpop.
    initialize = function(subpop, value) {
      # Can be character (referring to a column) or a function.
      if (is.character(subpop)) {
        self$subpop = function(rw) {rw[[subpop]]} # nocov
      } else {
        self$subpop = assert_function(subpop)
      }
      self$value = assert_number(value)
      invisible(self)
    },
    #' @description
    #' Fit the predictor.
    #' @template params_data_label
    fit = function(data, labels) {},
    #' @description
    #' Predict a dataset with sub-population predictions.
    #' @param data [`data.table`] \cr
    #'   Prediction data.
    #' @param ... [`any`] \cr
    #'   Not used, only for compatibility with other methods.
    predict = function(data, ...) {
      data[, self$subpop(.SD)] * self$value
    }
  )
)


#' SubgroupModel
#' @export
SubgroupModel = R6::R6Class("SubgroupModel",
  public = list(
    #' @field subgroup_masks [`list`] \cr
    #'   List of subgroup masks.
    subgroup_masks = NULL,
    #' @field subgroup_preds [`list`] \cr
    #'   List of subgroup predictions after fitting.
    subgroup_preds = NULL,
    #' @description
    #' Instantiate a SubpopPredictor
    #' @param subgroup_masks [`list`] \cr
    #'   List of subgroup masks.
    initialize = function(subgroup_masks) {
      self$subgroup_masks = assert_list(subgroup_masks)
      invisible(self)
    },
    #' @description
    #' Fit the predictor.
    #' @template params_data_label
    fit = function(data, labels) {
      self$subgroup_preds = map(self$subgroup_masks, function(mask) {
        mean(labels[as.logical(mask)])
      })
    },
    #' @description
    #' Predict a dataset with sub-population predictions.
    #' @param data [`data.table`] \cr
    #'   Prediction data.
    #' @param subgroup_masks [`list`] \cr
    #'   List of subgroup masks for the data.
    #' @param partition_mask [`integer`] \cr
    #'   Mask defined by partitions.
    predict = function(data, subgroup_masks = NULL, partition_mask = NULL) {
      # Check that masks fit
      if (is.null(subgroup_masks)) {
        subgroup_masks = self$subgroup_masks
      }
      if (!all(map_lgl(subgroup_masks, function(x) {nrow(data) == length(x)}))) {
        stop("Length of subgroup masks must match length of data!\n
              Subgroups are currently not implemented for 'partition=TRUE'.")
      }
      # If no paritition mask, use all datapoints
      if (is.null(partition_mask)) partition_mask = rep(1L, nrow(data))
      # Predict
      preds = numeric(nrow(data))
      for (i in seq_along(self$subgroup_preds)) {
        preds[subgroup_masks[[i]] & partition_mask] = self$subgroup_preds[[i]]
      }
      return(preds)
    }
  )
)

#' LearnerCVPredictor
#' Wraps a mlr3 Learner into a `LearnerCVPredictor` object that can be used
#' with mcboost. Internally cross-validates predictions.
#' @export
CVLearnerPredictor = R6::R6Class("CVLearnerPredictor",
  inherit = Predictor,
  public = list(
    #' @field pipeop [`Learner`]\cr
    #' mlr3pipelines PipeOp used for fitting residuals.
    pipeop = NULL,
    #' @description
    #' Instantiate a LearnerPredictor
    #'
    #' @param learner [`Learner`]\cr
    #'   Learner used for train/predict.
    initialize = function(learner) {
      self$pipeop = mlr3pipelines::po("learner_cv", learner)
    },
    #' @description
    #' Fit the learner.
    #' @template params_data_label
    fit_transform = function(data, labels) {
      task = xy_to_task(data, labels)
      t = self$pipeop$train(list(task))$output
      return(as.matrix(t$data(cols=t$feature_names)))
    },
    #' @description
    #' Predict a dataset with leaner predictions.
    #' @param data [`data.table`] \cr
    #'   Prediction data.
    #' @param ... [`any`] \cr
    #'   Not used, only for compatibility with other methods.
    predict = function(data, ...) {
      task = xy_to_task(data, runif(NROW(data)))
      t = self$pipeop$predict(list(task))$output
      return(as.matrix(t$data(cols=t$feature_names)))
    }
  ),
  active = list(
    #' @field is_fitted [`logical`]\cr
    #' Whether the Learner is trained
    is_fitted = function() {
      !is.null(self$pipeop$state)
    }
  )
)