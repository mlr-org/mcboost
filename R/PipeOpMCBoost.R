#' @title Multi-Calibrate a Learner's Prediction
#'
#' @usage NULL
#' @name mlr_pipeops_mcboost
#' @format [`R6Class`] inheriting from [`mlr3pipelines::PipeOp`].
#'
#' @description
#' Post-process a learner prediction using multi-calibration.
#' For more details, please refer to \url{https://arxiv.org/pdf/1805.12317.pdf} (Kim et al. 2018)
#' or the help for [`MCBoost`].
#' If no `init_predictor` is provided, the preceding learner's predictions
#' corresponding to the `prediction` slot are used as an initial predictor for `MCBoost`.
#'
#' @section Construction:
#' ```
#' PipeOpMCBoost$new(id = "mcboost", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"threshold"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   See `MCBoost` for a comprehensive description of all hyperparameters.
#'
#' @section Input and Output Channels:
#' During training, the input and output are `"data"` and `"prediction"`, two [`TaskClassif`][mlr3::TaskClassif].
#' A [`PredictionClassif`][mlr3::PredictionClassif] is required as input and returned as output during prediction.
#'
#' @section State:
#' The `$state` is a `MCBoost` Object as obtained from `MCBoost$new()`.
#'
#' @section Parameters:
#' * `max_iter` :: `integer`\cr
#'   A integer specifying the number of multi-calibration rounds. Defaults to 5.
#'
#' @section Fields:
#' Only fields inherited from [`mlr3pipelines::PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`mlr3pipelines::PipeOp`].
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3pipelines)
#' # Attention: gunion inputs have to be in the correct order for now
#'  gr = gunion(list(
#'    "data" = po("nop"),
#'    "prediction" = po("learner_cv", lrn("classif.rpart"))
#'   )) %>>%
#'   PipeOpMCBoost$new()
#' tsk = tsk("sonar")
#' tid = sample(1:208, 108)
#' gr$train(tsk$clone()$filter(tid))
#' gr$predict(tsk$clone()$filter(setdiff(1:208, tid)))
#' }
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
      param_set = paradox::ParamSet$new(list(
        paradox::ParamInt$new("max_iter", lower = 0L, upper = Inf, default =5L, tags = "train"),
        paradox::ParamDbl$new("alpha", lower = 0, upper = 1, default = 1e-4, tags = "train"),
        paradox::ParamDbl$new("eta", lower = 0, upper = 1, default = 1, tags = "train"),
        paradox::ParamLgl$new("partition", tags = "train", default = TRUE),
        paradox::ParamInt$new("num_buckets", lower = 1, upper = Inf, default = 2L, tags = "train"),
        paradox::ParamLgl$new("rebucket", default = FALSE, tags = "train"),
        paradox::ParamLgl$new("multiplicative", default = TRUE, tags = "train"),
        paradox::ParamUty$new("auditor_fitter", default = NULL, tags = "train"),
        paradox::ParamUty$new("subpops", default = NULL, tags = "train"),
        paradox::ParamUty$new("default_model_class", default = ConstantPredictor, tags = "train"),
        paradox::ParamUty$new("init_predictor", default = NULL, tags = "train")
      ))
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

      args = self$param_set$get_values(tags = "train")

      if (is.null(args$init_predictor)) {
        # Construct an initial predictor from the input model if non is provided.
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
        args$init_predictor = init_predictor
      }
      mc = invoke(MCBoost$new, .args = args)
      mc$multicalibrate(d, l, predictor_args = inputs$prediction)
      self$state = list("mc" = mc)
      list(NULL)
    },

    .predict = function(inputs) {
      d = inputs$data$data(cols = inputs$data$feature_names)
      prob = self$state$mc$predict_probs(d, predictor_args = inputs$prediction)
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


#' Multi-calibration pipeline
#'
#' Wraps MCBoost in a Pipeline to be used with `mlr3pipelines`.
#' For now this assumes training on the same dataset that is later used
#' for multi-calibration.
#' @param learner (mlr3)[`mlr3::Learner`]\cr
#'   Initial learner. Internally wrapped into a `PipeOpLearnerCV`
#'   with `resampling.method = "insample"` as a default.
#'   All parameters can be adjusted through the resulting Graph's `param_set`.
#'   Defaults to `lrn("classif.featureless")`.
#'   Note: An initial predictor can also be supplied via the `init_predictor` parameter.
#' @return (mlr3pipelines) [`Graph`]
#' @examples
#'   \dontrun{
#'   library("mlr3pipelines")
#'   gr = ppl_mcboost()
#'   }
#' @export
ppl_mcboost = function(learner = lrn("classif.featureless")) {
  mlr3misc::require_namespaces("mlr3pipelines")
  po_lrn = mlr3pipelines::po("learner_cv", learner = learner, resampling.method = "insample")
  gr = mlr3pipelines::`%>>%`(
    mlr3pipelines::gunion(list(
    "data" = mlr3pipelines::po("nop"),
    "prediction" = po_lrn
    )),
    PipeOpMCBoost$new()
  )
}
