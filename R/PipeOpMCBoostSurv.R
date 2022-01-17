#' @title Multi-Calibrate a Learner's Prediction (Survival Model)
#'
#' @usage NULL
#' @name mlr_pipeops_mcboostsurv
#' @format [`R6Class`] inheriting from [`mlr3pipelines::PipeOp`].
#'
#' @description
#' Post-process a survival learner prediction using multi-calibration.
#' For more details, please refer to \url{https://arxiv.org/pdf/1805.12317.pdf} (Kim et al. 2018)
#' or the help for [`MCBoostSurv`].
#' If no `init_predictor` is provided, the preceding learner's predictions
#' corresponding to the `prediction` slot are used as an initial predictor for `MCBoostSurv`.
#'
#' @section Construction:
#' ```
#' PipeOpMCBoostSurv$new(id = "mcboostsurv", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"threshold"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction.
#'   See `MCBoostSurv` for a comprehensive description of all hyperparameters.
#'
#' @section Input and Output Channels:
#' During training, the input and output are `"data"` and `"prediction"`, two [`TaskSurv`][mlr3proba::TaskSurv].
#' A [`PredictionSurv`][mlr3proba::PredictionSurv] is required as input and returned as output during prediction.
#'
#' @section State:
#' The `$state` is a `MCBoostSurv` Object as obtained from `MCBoostSurv$new()`.
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
#' library(mlr3)
#' library(mlr3pipelines)
#' # Attention: gunion inputs have to be in the correct order for now.
#' \dontrun{
#' gr = gunion(list(
#'   "data" = po("nop"),
#'   "prediction" = po("learner_pred", lrn("surv.ranger"))
#' )) %>>%
#'   PipeOpMCBoostSurv$new()
#' tsk = tsk("rats")
#' tid = sample(1:300, 100)
#' gr$train(tsk$clone()$filter(tid))
#' gr$predict(tsk$clone()$filter(setdiff(1:300, tid)))
#' }
#' @family PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
PipeOpMCBoostSurv = R6Class("PipeOpMCBoostSurv",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #'   Initialize a Multi-Calibration PipeOp (for Survival).
    #'
    #' @param id [`character`] \cr
    #'   The `PipeOp`'s id. Defaults to "mcboostsurv".
    #' @param param_vals [`list`] \cr
    #'   List of hyperparameters for the `PipeOp`.
    initialize = function(id = "mcboostsurv", param_vals = list()) {
      param_set = paradox::ParamSet$new(list(
        paradox::ParamInt$new("max_iter", lower = 0L, upper = Inf, default = 5L, tags = "train"),
        paradox::ParamDbl$new("alpha", lower = 0, upper = 1, default = 1e-4, tags = "train"),
        paradox::ParamDbl$new("eta", lower = 0, upper = 1, default = 1, tags = "train"),
        paradox::ParamInt$new("num_buckets", lower = 1, upper = Inf, default = 2L, tags = "train"),
        paradox::ParamInt$new("time_buckets", lower = 1, upper = Inf, default = 1L, tags = "train"),
        paradox::ParamDbl$new("time_eval", lower = 0, upper = 1, default = 1, tags = "train"),
        paradox::ParamUty$new("bucket_strategy", default = "quantiles", tags = "train"),
        paradox::ParamUty$new("bucket_aggregation", default = NULL, tags = "train"),
        paradox::ParamLgl$new("eval_fulldata", default = FALSE, tags = "train"),
        paradox::ParamLgl$new("rebucket", default = FALSE, tags = "train"),
        paradox::ParamLgl$new("multiplicative", default = TRUE, tags = "train"),
        paradox::ParamUty$new("auditor_fitter", default = NULL, tags = "train"),
        paradox::ParamUty$new("subpops", default = NULL, tags = "train"),
        paradox::ParamUty$new("default_model_class", default = LearnerSurvKaplan, tags = "train"),
        paradox::ParamUty$new("init_predictor", default = NULL, tags = "train")
      ))
      super$initialize(id,
        param_set = param_set, param_vals = param_vals, packages = c("mlr3proba", "survival"),
        input = data.table(
          name = c("data", "prediction"), 
          train = c("TaskSurv", "TaskSurv"),
          predict = c("TaskSurv", "TaskSurv")
        ),
        output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
        tags = "target transform")
    }
  ),
  private = list(
    .train = function(inputs) {

      d = inputs$data$data(cols = inputs$data$feature_names)
      l = inputs$data$data(cols = inputs$data$target_names)

      args = self$param_set$get_values(tags = "train")

      if (is.null(args$init_predictor)) {
        # Construct an initial predictor from the input model if none is provided.
        init_predictor = function(data, prediction) {
          distr_col = prediction$feature_names[grepl("distr$",prediction$feature_names)]
          if (is.null(distr_col)) stop("No distr output in the predictions.")
          if (length(distr_col) > 1) stop("More than one distr columns in the prediction?")
          as.data.table(prediction)[[distr_col]][[1]][[1]]
        }
        args$init_predictor = init_predictor
      }
      mc = mlr3misc::invoke(MCBoostSurv$new, .args = args)
      mc$multicalibrate(d, l, predictor_args = inputs$prediction)
      self$state = list("mc" = mc)
      list(NULL)
    },

    .predict = function(inputs) {
      d = inputs$data$data(cols = inputs$data$feature_names)
      probs = as.matrix(self$state$mc$predict_probs(d, predictor_args = inputs$prediction))

      time = as.numeric(colnames(probs))

      list(PredictionSurv$new(
        truth = inputs$prediction$truth(),
        distr = probs,
        row_ids = inputs$prediction$row_ids,
        crank = -apply(1 - probs, 1, function(.x) sum(c(.x[1], diff(.x)) * time))
      ))

    }
  ),
  active = list(
    #' @field predict_type Predict type of the PipeOp.
    predict_type = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$predict_type for PipeOpMCBoostSurv is read-only.")
        }
      }
      return("distr")
    }
  )
)
