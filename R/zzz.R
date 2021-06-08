#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import glmnet
#' @import rpart
#' @importFrom R6 R6Class is.R6
#' @importFrom utils head
#' @importFrom stats contrasts runif rnorm  setNames
#' @references
#'   Kim et al., 2019: Multiaccuracy: Black-Box Post-Processing for Fairness in Classification.
#'   Hebert-Johnson et al., 2018: Multicalibration: Calibration for the ({C}omputationally-Identifiable) Masses.
"_PACKAGE"


register_pipeops = function() {
  mlr3pipelines::mlr_pipeops$add("mcboost", PipeOpMCBoost)
}

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
