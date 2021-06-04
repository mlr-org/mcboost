#' ResidualFitter Abstract Base Class
#' @family SubPopFitters
#' @export
ResidualFitter = R6::R6Class("ResidualFitter",
  public = list(
    #' @description
    #' Fit to residuals
    #' @template params_data_resid
    #' @template params_mask
    fit_to_resid = function(data, resid, mask) {
      # Learners fail on constant residuals.
      if (all(unique(resid) == resid[1])) {
        return(list(0, ConstantPredictor$new(0)))
      }
      self$fit(data, resid, mask)
    },
    #' @description
    #' Fit to residuals
    #' @template params_data_resid
    #' @template params_mask
    fit = function(data, resid, mask) {
      stop("Not implemented")
    }
  )
)

#' ResidualFitter from a Learner
#' @family SubPopFitters
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
    #' @template params_data_resid
    #' @template params_mask
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
#' @family SubPopFitters
#' @export
TreeResidualFitter = R6::R6Class("TreeResidualFitter",
  inherit = LearnerResidualFitter,
  public = list(
    #' @description
    #' Define a ResidualFitter from a rpart learner
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "rpart"))
      super$initialize(learner = lrn("regr.rpart"))
    }
  )
)

#' @describeIn LearnerResidualFitter ResidualFitter based on glmnet
#' @family SubPopFitters
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
#' @family SubPopFitters
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
    #' Initialize SubPopFitter
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
    #' @template params_data_resid
    #' @template params_mask
    #' @return `list` with items\cr
    #'   - `corr`: pseudo-correlation between residuals and learner prediction.
    #'   - `l`: the trained learner.
    fit = function(data, resid, mask) {
      worstCorr = 0
      worst_subpop = function(pt) {return(rep(0L, nrow(pt)))} # nocov
      for (sfn in self$subpops) {
        sub = data[, sfn(.SD)]
        corr = mean(sub * resid)
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
#' @family SubPopFitters
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
      subgroup_masks = tryCatch({map(subgroup_masks, as.integer)},
        warning = function(w) {
          stop("subgroup_masks must be a list of integers.")
        })
      self$subgroup_masks = assert_list(subgroup_masks, types = "integer")
      if (!all(map_lgl(self$subgroup_masks, function(x) {test_numeric(x, lower = 0, upper = 1)}))) {
        stop("subgroup_masks must be binary vectors")
      }
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @template params_data_resid
    #' @template params_mask
    #' @return `list` with items\cr
    #'   - `corr`: pseudo-correlation between residuals and learner prediction.
    #'   - `l`: the trained learner.
    fit = function(data, resid, mask) {
      sg = map(self$subgroup_masks, function(x) x[mask])
      if (!all(map_lgl(sg, function(x) {nrow(data) == length(x)}))) {
        stop("Length of subgroup masks must match length of data!")
      }
      m = SubgroupModel$new(sg)
      m$fit(data, resid)
      preds = m$predict(data)
      corr = mean(preds*resid)
      return(list(corr, m))
    }
  )
)

#' Cross-validated ResidualFitter from a Learner
#' @description CVLearnerResidualFitter returns the cross-validated predictions
#' instead of the in-sample predictions.
#'
#' Available data is cut into complementary subsets (folds).
#' For each subset out-of-sample predictions are received by training a model
#' on all other subset and predicting afterwards on the left-out subset.
#' @family SubPopFitters
#' @export
CVLearnerResidualFitter = R6::R6Class("CVLearnerResidualFitter",
  inherit = ResidualFitter,
  public = list(
    #' @field learner [`CVLearnerPredictor`]\cr
    #' Learner used for fitting residuals.
    learner = NULL,
    #' @description
    #' Define a CVResidualFitter from a Learner.
    #' Available instantiations: [`CVTreeResidualFitter`] (rpart) and
    #' [`CVRidgeResidualFitter`] (glmnet).
    #' See [`mlr3pipelines::PipeOpLearnerCV`] for more information on
    #' cross-validated learners.
    #'
    #' @param learner [`Learner`]\cr
    #' Regression Learner to use.
    #' @param folds [`integer`]\cr
    #'   Number of folds to use for PipeOpLearnerCV. Default: 3.
    initialize = function(learner, folds = 3L) {
      self$learner = CVLearnerPredictor$new(learner, folds)
    },
    #' @description
    #' Fit the cv-learner and compute correlation
    #'
    #' @template params_data_resid
    #' @template params_mask
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

#' @describeIn CVLearnerResidualFitter Cross-Validated ResidualFitter based on rpart
#' @family SubPopFitters
#' @export
CVTreeResidualFitter = R6::R6Class("CVTreeResidualFitter",
  inherit = CVLearnerResidualFitter,
  public = list(
    #' @description
    #' Define a cross-validated ResidualFitter from a rpart learner
    #' See [`mlr3pipelines::PipeOpLearnerCV`] for more information on
    #' cross-validated learners.
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "rpart"))
      super$initialize(learner = lrn("regr.rpart"))
    }
  )
)

#' @describeIn CVLearnerResidualFitter Cross-Validated ResidualFitter based on glmnet
#' @family SubPopFitters
#' @export
CVRidgeResidualFitter = R6::R6Class("CVRidgeResidualFitter",
  inherit = CVLearnerResidualFitter,
  public = list(
    #' @description
    #' Define a cross-validated ResidualFitter from a glmnet learner
    #' See [`mlr3pipelines::PipeOpLearnerCV`] for more information on
    #' cross-validated learners.
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "glmnet"))
      super$initialize(learner = lrn("regr.glmnet", alpha = 0))
    }
  )
)
