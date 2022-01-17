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
#' @param param_vals `list` \cr
#'   List of parameter values passed on to `MCBoost$new`.
#' @return (mlr3pipelines) [`Graph`]
#' @examples
#'   \dontrun{
#'   library("mlr3pipelines")
#'   gr = ppl_mcboost()
#'   }
#' @export
ppl_mcboost = function(learner = lrn("classif.featureless"), param_vals = list()) {
  mlr3misc::require_namespaces("mlr3pipelines")
  po_lrn = mlr3pipelines::po("learner_cv", learner = learner, resampling.method = "insample")
  gr = mlr3pipelines::`%>>%`(
    mlr3pipelines::gunion(list(
      "data" = mlr3pipelines::po("nop"),
      "prediction" = po_lrn
    )),
    PipeOpMCBoost$new(param_vals = param_vals)
  )
}


#' Multi-calibration pipeline (for survival models)
#'
#' Wraps MCBoostSurv in a Pipeline to be used with `mlr3pipelines`.
#' For now this assumes training on the same dataset that is later used
#' for multi-calibration.
#' @param learner (mlr3)[`mlr3::Learner`]\cr
#'   Initial learner.
#'   Defaults to `lrn("surv.kaplan")`.
#'   Note: An initial predictor can also be supplied via the `init_predictor` parameter.
#'   The learner is internally wrapped into a `PipeOpLearnerCV`
#'   with `resampling.method = "insample"` as a default.
#'   All parameters can be adjusted through the resulting Graph's `param_set`.
#' @param param_vals `list` \cr
#'   List of parameter values passed on to `MCBoostSurv$new`
#' @return (mlr3pipelines) [`Graph`]
#' @examples
#' library("mlr3pipelines")
#' gr = ppl_mcboostsurv()
#' @export
ppl_mcboostsurv = function(learner = lrn("surv.kaplan"), param_vals = list()) {
  mlr3misc::require_namespaces("mlr3pipelines")
  mlr3misc::require_namespaces("mlr3proba")
  gr = mlr3pipelines::`%>>%`(
    mlr3pipelines::gunion(list(
      "data" = mlr3pipelines::po("nop"),
      "prediction" =  mlr3pipelines::po("learner_pred", learner = learner)
    )),
    PipeOpMCBoostSurv$new(param_vals = param_vals)
  )
}
