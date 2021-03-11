#' @title Multi-Calibrate a Learner's Prediction
#'
#' @usage NULL
#' @name mlr_pipeops_mcboost
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Post-process a learner prediction using multi-calibration.
#' For more details, please refer to \url{https://arxiv.org/pdf/1805.12317.pdf} (Kim et al. 2018).
#' The preceeding learner's predictions are used as an initial predictor for mcboost.
#'
#' @section Construction:
#' ```
#' PipeOpMCBoost$new(id = "mcboost", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"threshold"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   Defaults to `numeric(0)`.
#'
#' @section Input and Output Channels:
#' During training, the input and output are `"data"` and `"prediction"`, two [`TaskClassif`][mlr3::TaskClassif].
#' A [`PredictionClassif`][mlr3::PredictionClassif] is required as input and returned as output during prediction.
#'
#' @section State:
#' The `$state` is a `MCBoost` Object as obained from [`MCBoost$new()`].
#'
#' @section Parameters:
#' * `max_iter` :: `integer`\cr
#'   A integer specifying the number of multi-calibration rounds. Defaults to 5.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' library(mlr3)
#' library(mlr3pipelines)
#' # Attention: gunion inputs have to be in the correct order for now.
#'  gr = gunion(list(
#'    "data" = po("nop"),
#'    "prediction" = po("learner_cv", lrn("classif.rpart"))
#'   )) %>>%
#'   PipeOpMCBoost$new()
#' tsk = tsk("sonar")
#' tid = sample(1:208, 108)
#' gr$train(tsk$clone()$filter(tid))
#' gr$predict(tsk$clone()$filter(setdiff(1:208, tid)))
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
PipeOpMCBoost = R6Class("PipeOpMCBoost",
  inherit = mlr3pipelines::PipeOp,
  public = list(

    #' @description
    #'   Initialize a Multi-Calibration PipeOp.
    #'
    #' @param id [`character`] \cr
    #'   The `PipeOp`'s id. Defaults to "mcboost".
    #' @param param_vals [`list`] \cr
    #'   List of hyperparameters for the `PipeOp`.
    initialize = function(id = "mcboost", param_vals = list()) {
      param_set = paradox::ParamSet$new()
      param_set$add(paradox::ParamInt$new("max_iter", lower = 0L, upper = Inf, tags = "train"))
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = character(0),
        input = data.table(name = c("data", "prediction"), train = c("TaskClassif", "TaskClassif"), predict = c("TaskClassif", "TaskClassif")),
        output = data.table(name = "output", train = "NULL", predict = "PredictionClassif"),
        tags = "target transform")
    }

  ),
  private = list(
    .train = function(inputs) {
      d = inputs$data$data(cols = inputs$data$feature_names)
      l = inputs$data$data(cols = inputs$data$target_names)

      init_predictor = function(data, prediction) {
        # Prob or response prediction
        if (length(prediction$feature_names) > 1L) {
          prds = prediction$data(cols = prediction$feature_names)
          as.matrix(prds)
        } else {
          prds = prediction$data(cols = prediction$feature_names)[[1]]
          one_hot(prds)
        }
      }

      mc = MCBoost$new(init_predictor = init_predictor)
      mc$multicalibrate(d, l, prediction = inputs$prediction)
      self$state = list("mc" = mc)
      list(NULL)
    },
    .predict = function(inputs) {
      d = inputs$data$data(cols = inputs$data$feature_names)
      prob = self$state$mc$predict_probs(d, prediction = inputs$prediction)
      prob = cbind(1 - prob, prob)
      lvls = c(inputs$prediction$negative, inputs$prediction$positive)
      colnames(prob) = lvls
      list(PredictionClassif$new(
        inputs$prediction,
        row_ids = inputs$prediction$row_ids,
        truth = inputs$prediction$truth(),
        prob = prob
      ))
    }
  ),
  active = list(
    #' @field predict_type Predict type of the PipeOp.
    predict_type = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$predict_type for PipeOpMCBoost is read-only.")
        }
      }
      return("prob")
    }
  )
)

mlr_pipeops$add("mcboost", PipeOpMCBoost)
