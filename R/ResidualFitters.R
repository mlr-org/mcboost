#' ResidualFitter Abstract Base Class
#' @export
ResidualFitter = R6::R6Class("ResidualFitter",
  public = list(
    #' @description
    #' Fit to residuals
    #' @template params_data_resid
    fit_to_resid = function(data, resid) {
      self$fit(data, resid)
    },
    #' @description
    #' Fit to residuals
    #' @template params_data_resid
    fit = function(data, resid) {
      stop("Not implemented")
    }
  )
)

#' ResidualFitter from a Learner
#' @export
LearnerResidualFitter = R6::R6Class("LearnerResidualFitter",
  inherit = ResidualFitter,
  public = list(
    #' @field learner [`LearnerPredictor`]\cr
    #' Learner used for fitting residuals.
    learner = NULL,
    #' @description
    #' Define a ResidualFitter from a Learner
    #' Available instantiations: [`TreeResidualFitter`] (rpart) and
    #' [`RidgeResidualFitter`] (glmnet).
    #'
    #' @param learner [`Learner`]\cr
    #' Regression Learner to use.
    initialize = function(learner) {
      self$learner = LearnerPredictor$new(learner)
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @param data [`data.frame`]\cr
    #'   Features to use.
    #' @param resid [`numeric`]\cr
    #'   Target variable (residuals)
    #' @return `list`
    fit = function(data, resid) {
      l = self$learner$clone()
      l$fit(data, resid)
      h = l$predict(data)
      corr = mean(h*resid)
      return(list(corr, l))
    }
  )
)

#' @describeIn LearnerResidualFitter ResidualFitter based on rpart
#' @export
TreeResidualFitter = R6::R6Class("TreeResidualFitter",
  inherit = LearnerResidualFitter,
  public = list(
    #' @description
    #' Define a ResidualFitter from a rpart learner
    initialize = function() {
      super$initialize(learner = lrn("regr.rpart"))
    }
  )
)

#' @describeIn LearnerResidualFitter ResidualFitter based on glmnet
#' @export
RidgeResidualFitter = R6::R6Class("RidgeResidualFitter",
  inherit = LearnerResidualFitter,
  public = list(
    #' @description
    #' Define a ResidualFitter from a glmnet learner
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "glmnet"))
      super$initialize(learner = lrn("regr.glmnet", alpha = 0))
    }
  )
)

#' Static ResidualFitter based on Subpopulations
#' @export
SubPopFitter = R6::R6Class("SubPopFitter",
  inherit = ResidualFitter,
  public = list(
    #' @description
    #' Initialize SubPopFilter
    #'
    #' @template params_subpops
    initialize = function(subpops) {
      self$subpops = assert_list(subpops, names = "named")
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @param data [`data.frame`]\cr
    #'   Features to use.
    #' @param resid [`numeric`]\cr
    #'   Target variable (residuals)
    #' @return `list`
    fit = function(data, resid) {
      worstCorr = 0
      worst_subpop = function(pt) {return(0)}

      for (s in subpops) {
        sub = apply(data, 1, s)
        corr = mean(sub * resid)

        if (abs(corr) > abs(worstCor)) {
          worstCorr = corr
          worst_subpop = S
        }
        return(list(worstCorr, SubpopPredictor$new(worst_subpop, worstCorr)))
      }
    }
  )
)

#' Static ResidualFitter based on Subgroups
#' @export
SubgroupFitter = R6::R6Class("SubgroupFitter",
  inherit = ResidualFitter,
  public = list(
    #' @field subgroup_masks [`list`] \cr
    #'   List of subgroup masks.
    subgroup_masks = NULL,
    #' @description
    #' Initialize SubgroupFitter
    #'
    #' @param subgroup_masks [`list`] \cr
    #'   List of subgroup masks.
    initialize = function(subgroup_masks) {
      self$subgroup_masks = assert_list(subgroup_masks, names = "named")
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @param data [`data.frame`]\cr
    #'   Features to use.
    #' @param resid [`numeric`]\cr
    #'   Target variable (residuals)
    #' @return `list`
    fit = function(data, resid) {
      m = SubgroupModel$new(self$subgroup_masks)
      m$fit(data, resid)
      preds = m$predict(data)
      corr = mean(preds*resid)
      return(list(corr, m))
    }
  )
)
