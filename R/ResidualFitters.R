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
    #' @param mask [`integer`]\cr
    #'   Mask applied to the data.
    fit = function(data, resid, mask) {
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
    #'   Target variable (residuals).
    #' @param mask [`integer`]\cr
    #'   Mask applied to the data.
    #' @return `list` with items\cr
    #'   - `corr`: pseudo-correlation between residuals and learner prediction.
    #'   - `l`: the trained learner.
    fit = function(data, resid, mask) {
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
SubpopFitter = R6::R6Class("SubpopFitter",
  inherit = ResidualFitter,
  public = list(
    #' @field subgroup_masks [`list`] \cr
    #'   List of subgroup masks.
    subgroup_masks = NULL,
    #' @field subpops [`list`] \cr
    #'   List of subpopulation indicators.
    subpops = NULL,
    #' @description
    #' Initialize SubPopFilter
    #'
    #' @template params_subpops
    initialize = function(subpops) {
      assert_list(subpops)
      self$subpops = map(subpops, function(pop) {
        # Can be character (referring to a column)
        if (is.character(pop)) {
          function(rw) {rw[[pop]]}
        } else {
          assert_function(pop)
        }
      })
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @param data [`data.frame`]\cr
    #'   Features to use.
    #' @param resid [`numeric`]\cr
    #'   Target variable (residuals).
    #' @param mask [`integer`]\cr
    #'   Mask applied to the data.
    #' @return `list` with items\cr
    #'   - `corr`: pseudo-correlation between residuals and learner prediction.
    #'   - `l`: the trained learner.
    fit = function(data, resid, mask) {
      worstCorr = 0
      worst_subpop = function(pt) {return(rep(0L, nrow(pt)))} # nocov
      for (sfn in self$subpops) {
        sub = data[, sfn(.SD)]
        corr = mean(sub[mask] * resid[mask])
        if (abs(corr) > abs(worstCorr)) {
          worstCorr = corr
          worst_subpop = sfn
        }
      }
      return(list(worstCorr, SubpopPredictor$new(worst_subpop, worstCorr)))
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
    #'   List of subgroup masks. Subgroup masks are list(s) of integer masks,
    #'   each with the same length as data to be fitted on.
    #'   They allow defining sub-groups of the data.
    initialize = function(subgroup_masks) {
      self$subgroup_masks = assert_list(map(subgroup_masks, as.integer), types = "integer")
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @param data [`data.frame`]\cr
    #'   Features to use.
    #' @param resid [`numeric`]\cr
    #'   Target variable (residuals).
    #' @param mask [`integer`]\cr
    #'   Mask applied to the data.
    #' @return `list` with items\cr
    #'   - `corr`: pseudo-correlation between residuals and learner prediction.
    #'   - `l`: the trained learner.
    fit = function(data, resid, mask) {
      sg = map(self$subgroup_masks, function(x) x[mask])
      m = SubgroupModel$new(sg)
      m$fit(data, resid)
      preds = m$predict(data)
      corr = mean(preds*resid)
      return(list(corr, m))
    }
  )
)

#' ResidualFitter from a Learner
#' @export
CVLearnerResidualFitter = R6::R6Class("CVLearnerResidualFitter",
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
      self$learner = CVLearnerPredictor$new(learner)
    },
    #' @description
    #' Fit the cv-learner and compute correlation
    #'
    #' @param data [`data.frame`]\cr
    #'   Features to use.
    #' @param resid [`numeric`]\cr
    #'   Target variable (residuals).
    #' @param mask [`integer`]\cr
    #'   Mask applied to the data.
    #' @return `list` with items\cr
    #'   - `corr`: pseudo-correlation between residuals and learner prediction.
    #'   - `l`: the trained learner.
    fit = function(data, resid, mask) {
      l = self$learner$clone()
      h = l$fit_transform(data, resid)
      corr = mean(h*resid)
      return(list(corr, l))
    }
  )
)

#' @describeIn LearnerResidualFitter Cross-Validated residualFitter based on rpart
#' @export
CVTreeResidualFitter = R6::R6Class("CVTreeResidualFitter",
  inherit = CVLearnerResidualFitter,
  public = list(
    #' @description
    #' Define a ResidualFitter from a rpart learner
    initialize = function() {
      super$initialize(learner = lrn("regr.rpart"))
    }
  )
)

#' @describeIn LearnerResidualFitter Cross-Validated ResidualFitter based on glmnet
#' @export
CVRidgeResidualFitter = R6::R6Class("CVRidgeResidualFitter",
  inherit = CVLearnerResidualFitter,
  public = list(
    #' @description
    #' Define a ResidualFitter from a glmnet learner
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "glmnet"))
      super$initialize(learner = lrn("regr.glmnet", alpha = 0))
    }
  )
)
