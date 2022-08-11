#' @title Multi-Calibrate a Learner's Prediction
#'
#' @usage NULL
#' @name mlr_pipeops_mcboost
#' @format [`R6Class`] inheriting from [`mlr3pipelines::PipeOp`].
#'
#' @description
#'   [`mlr3pipelines::PipeOp`] that trains a [`Learner`][mlr3::Learner] and passes its predictions forward during training and prediction.
#'
#' @section Construction:
#' ```
#'PipeOpLearnerPred$new(learner, id = NULL, param_vals = list())
#'
#' * `learner` :: [`Learner`][mlr3::Learner] \cr
#'   [`Learner`][mlr3::Learner] to  prediction, or a string identifying a
#'   [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpLearnerPred`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearnerPred`] has one output channel named `"output"`, producing a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' @section State:
#
#' @section Parameters:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object, together with the `$state` elements inherited from
#' [`mlr3pipelines::PipeOpTaskPreproc`]. It is a named `list` with the inherited members, as well as:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
#' * `train_log` :: [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_model` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. This learner contains the model if the `PipeOp` is trained. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`mlr3pipelines::PipeOpTaskPreproc`]/[`mlr3pipelines::PipeOp`].
#'
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
PipeOpLearnerPred = R6Class("PipeOpLearnerPred",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description
    #'   Initialize a Learner Predictor PipeOp. Can be used to wrap trained or untrainted 
    #'   mlr3 learners.
    #' @param learner [`Learner`]\cr
    #'   The learner that should be wrapped.  
    #' @param id [`character`] \cr
    #'   The `PipeOp`'s id. Defaults to "mcboost".
    #' @param param_vals [`list`] \cr
    #'   List of hyperparameters for the `PipeOp`.
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      private$.learner$param_set$set_id = ""
      id = id %??% private$.learner$id
      task_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$task
      super$initialize(id, alist(private$.learner$param_set),
        param_vals = param_vals,
        can_subset_cols = TRUE,
        task_type = task_type,
        tags = c("learner")
      )
    }

  ),
  active = list(
    #' @field learner The wrapped learner.
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      private$.learner
    },
    #' @field learner_model The wrapped learner's model(s).
    learner_model = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      if (is.null(self$state) || mlr3pipelines::is_noop(self$state)) {
        private$.learner
      } else {
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learner)
      }
    }
  ),
  private = list(
    .train_task = function(task) {
      on.exit({private$.learner$state = NULL})

      # Train a learner for predicting
      state = private$.learner$state
      if (is.null(state)) {
        self$state = private$.learner$train(task)$state
      } else {
        self$state = state
      }

      prds = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prds, task)
    },

    .predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      private$.learner$state = self$state
      prediction = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prediction, task)
    },

    pred_to_task = function(prds, task) {
      renaming = setdiff(colnames(prds), c( "row_ids"))
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))
      setnames(prds, old = "row_ids", new = task$backend$primary_key)
      task$select(character(0))$cbind(prds)
    },
    .learner = NULL
  )
)

clone_with_state = function(learner, state) {
  lrn = learner$clone(deep = TRUE)
  lrn$state = state
  lrn
}

multiplicity_recurse = function(.multip, .fun, ...) {
  if (mlr3pipelines::is.Multiplicity(.multip)) {
    mlr3pipelines::as.Multiplicity(lapply(.multip, function(m) multiplicity_recurse(.multip = m, .fun = .fun, ...)))
  } else {
    .fun(.multip, ...)
  }
}
