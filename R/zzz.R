#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import glmnet
#' @import rpart
#' @import mlr3proba
#' @import mlr3pipelines
#' @importFrom R6 R6Class is.R6
#' @importFrom utils head
#' @importFrom stats contrasts runif rnorm  setNames quantile
#' @references
#'   Kim et al., 2019: Multiaccuracy: Black-Box Post-Processing for Fairness in Classification.
#'   Hebert-Johnson et al., 2018: Multicalibration: Calibration for the ({C}omputationally-Identifiable) Masses.
#'   `r tools::toRd(citation("mcboost"))`
"_PACKAGE"


register_pipeops = function() { # nocov start
  mlr3pipelines::mlr_pipeops$add("mcboost", PipeOpMCBoost)
  mlr3pipelines::mlr_pipeops$add("mcboostsurv", PipeOpMCBoostSurv)
  mlr3pipelines::mlr_pipeops$add("learner_pred", PipeOpLearnerPred)
  mlr3pipelines::mlr_graphs$add("ppl_mcboost", ppl_mcboost)
  mlr3pipelines::mlr_graphs$add("ppl_mcboostsurv", ppl_mcboostsurv)
} # nocov end

.onLoad = function(libname, pkgname) {  # nocov start
  if (requireNamespace("mlr3pipelines")) {
    register_pipeops()
    setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_pipeops(), action = "append")
  }
  backports::import(pkgname)
}  # nocov end

.onUnload = function(libpath) { # nocov start
  if (requireNamespace("mlr3pipelines")) {
    event = packageEvent("mlr3pipelines", "onLoad")
    hooks = getHook(event)
    pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
    setHook(event, hooks[pkgname != "mcboost"], action = "replace")
  }
} # nocov end

leanify_package()
