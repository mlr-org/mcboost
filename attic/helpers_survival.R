#' Make every row monotonically decreasing in order to obtain the survival property.
#' Additionally, many predicitions need 1 as a first value and 0 as a last value.
#' (e.g. `PredictionSurv` needs this attribute.)
#' @param prediction [`data.table`]
#' Data.table with predictions. Every row is survival probability for the corresponding time.
#' Every column corresponds to  a specific time point.
#' @return [`data.table`]
#' @export
make_survival_curve = function(prediction) {
  survival_curves = apply(prediction, 1, function(x) {
    x[is.na(x)] = 0
    cm = cummin(x)
    if (any(x != cm)) {
      cm
    }else{
      x
    }
  })
  survival_curves = t(survival_curves)

  #needed for PredictionSurv
  survival_curves[,1]=1
  survival_curves[,ncol(survival_curves)]=0

  as.data.table(survival_curves)
}

#   if (inherits(y, "Surv")) {
#     ti = mlr3proba::TaskSurv
#   } else