#' AuditorFitter Abstract Base Class
#' @description
#'   Defines an `AuditorFitter` abstract base class.
#' @export
AuditorFitter = R6::R6Class("AuditorFitter",
  public = list(
    #' @description
    #' Initialize a [`AuditorFitter`].
    #' This is an abstract base class.
    initialize = function() {},
    #' @description
    #' Fit to residuals.
    #' @template params_data_resid
    #' @template params_mask
    #' @template return_fit
    fit_to_resid = function(data, resid, mask) {
      # Learners fail on constant residuals.
      if (all(unique(resid) == resid[1])) {
        return(list(0, ConstantPredictor$new(0)))
      }
      self$fit(data, resid, mask)
    },
    #' @description
    #' Fit (mostly used internally, use `fit_to_resid`).
    fit = function(data, resid, mask) {
      stop("Not implemented")
    }
  )
)

#' Create an AuditorFitter from a Learner
#' @description
#'   Instantiates an AuditorFitter that trains a [`mlr3::Learner`]
#'   on the data.
#' @family AuditorFitter
#' @export
LearnerAuditorFitter = R6::R6Class("LearnerAuditorFitter",
  inherit = AuditorFitter,
  public = list(
    #' @field learner `LearnerPredictor`\cr
    #' Learner used for fitting residuals.
    learner = NULL,
    #' @description
    #' Define an `AuditorFitter` from a Learner.
    #' Available instantiations:\cr [`TreeAuditorFitter`] (rpart) and
    #' [`RidgeAuditorFitter`] (glmnet).
    #'
    #' @param learner [`mlr3::Learner`]\cr
    #' Regression learner to use.
    #' @template return_auditor
    initialize = function(learner) {
      self$learner = LearnerPredictor$new(learner)
    },
    #' @description
    #' Fit the learner and compute correlation
    #'
    #' @template params_data_resid
    #' @template params_mask
    #' @template return_fit
    fit = function(data, resid, mask) {
      l = self$learner$clone()
      l$fit(data, resid)
      h = l$predict(data)
      corr = mean(h*resid)
      return(list(corr, l))
    }
  )
)

#' @describeIn LearnerAuditorFitter Learner auditor based on rpart
#' @family AuditorFitter
#' @export
TreeAuditorFitter = R6::R6Class("TreeAuditorFitter",
  inherit = LearnerAuditorFitter,
  public = list(
    #' @description
    #' Define a AuditorFitter from a rpart learner.
    initialize = function() {
      mlr3misc::require_namespaces("rpart")
      super$initialize(learner = lrn("regr.rpart"))
    }
  )
)

#' @describeIn LearnerAuditorFitter Learner auditor based on glmnet
#' @family AuditorFitter
#' @export
RidgeAuditorFitter = R6::R6Class("RidgeAuditorFitter",
  inherit = LearnerAuditorFitter,
  public = list(
    #' @description
    #' Define a AuditorFitter from a glmnet learner.
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "glmnet"))
      super$initialize(learner = lrn("regr.glmnet", alpha = 0))
    }
  )
)

#' Static AuditorFitter based on Subpopulations
#' @description
#'   Used to assess multi-calibration based on a list of
#'   binary valued columns: `subpops` passed during initialization.
#' @family AuditorFitter
#' @examples
#'   \dontrun{
#'   library("data.table")
#'   data = data.table(
#'     "AGE_NA" = c(0, 0, 0, 0, 0),
#'     "AGE_0_10" =  c(1, 1, 0, 0, 0),
#'     "AGE_11_20" = c(0, 0, 1, 0, 0),
#'     "AGE_21_31" = c(0, 0, 0, 1, 1),
#'     "X1" = runif(5),
#'     "X2" = runif(5)
#'   )
#'   label = c(1,0,0,1,1)
#'   pops = list("AGE_NA", "AGE_0_10", "AGE_11_20", "AGE_21_31", function(x) {x[["X1" > 0.5]]})
#'   sf = SubpopAuditorFitter$new(subpops = pops)
#'   sf$fit(data, label - 0.5)
#'   }
#' @export
SubpopAuditorFitter = R6::R6Class("SubpopAuditorFitter",
  inherit = AuditorFitter,
  public = list(
    #' @field subpops [`list`] \cr
    #'   List of subpopulation indicators.
    subpops = NULL,
    #' Initialize a SubpopAuditorFitter
    #' @description
    #' Initializes a [`SubpopAuditorFitter`] that
    #' assesses multi-calibration within each group defined
    #' by the `subpops'. Names in `subpops` must correspond to
    #' columns in the data.
    #'
    #' @template params_subpops
    #' @template return_auditor
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
    #' @template return_fit
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

#' @title Static AuditorFitter based on Subgroups
#' @description
#'   Used to assess multi-calibration based on a list of
#'   binary `subgroup_masks` passed during initialization.
#' @family AuditorFitter
#' @examples
#'  \dontrun{
#'  library("data.table")
#'  data = data.table(
#'    "AGE_0_10" =  c(1, 1, 0, 0, 0),
#'    "AGE_11_20" = c(0, 0, 1, 0, 0),
#'    "AGE_21_31" = c(0, 0, 0, 1, 1),
#'    "X1" = runif(5),
#'    "X2" = runif(5)
#'  )
#'  label = c(1,0,0,1,1)
#'  masks = list(
#'    "M1" = c(1L, 0L, 1L, 1L, 0L),
#'    "M2" = c(1L, 0L, 0L, 0L, 1L)
#'  )
#'  sg = SubgroupAuditorFitter$new(masks)
#'  }
#' @export
SubgroupAuditorFitter = R6::R6Class("SubgroupAuditorFitter",
  inherit = AuditorFitter,
  public = list(
    #' @field subgroup_masks [`list`] \cr
    #'   List of subgroup masks.
    subgroup_masks = NULL,
    #' Initialize a SubgroupAuditorFitter
    #' @description
    #' Initializes a [`SubgroupAuditorFitter`] that
    #' assesses multi-calibration within each group defined
    #' by the `subpops'.
    #'
    #' @param subgroup_masks [`list`] \cr
    #'   List of subgroup masks. Subgroup masks are list(s) of integer masks,
    #'   each with the same length as data to be fitted on.
    #'   They allow defining subgroups of the data.
    #' @template return_auditor
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
    #' @template return_fit
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

#' Cross-validated AuditorFitter from a Learner
#' @description CVLearnerAuditorFitter returns the cross-validated predictions
#' instead of the in-sample predictions.
#'
#' Available data is cut into complementary subsets (folds).
#' For each subset out-of-sample predictions are received by training a model
#' on all other subsets and predicting afterwards on the left-out subset.
#' @family AuditorFitter
#' @export
CVLearnerAuditorFitter = R6::R6Class("CVLearnerAuditorFitter",
  inherit = AuditorFitter,
  public = list(
    #' @field learner `CVLearnerPredictor`\cr
    #' Learner used for fitting residuals.
    learner = NULL,
    #' @description
    #' Define a `CVAuditorFitter` from a learner.
    #' Available instantiations:\cr [`CVTreeAuditorFitter`] (rpart) and
    #' [`CVRidgeAuditorFitter`] (glmnet).
    #' See [`mlr3pipelines::PipeOpLearnerCV`] for more information on
    #' cross-validated learners.
    #'
    #' @param learner [`mlr3::Learner`]\cr
    #' Regression Learner to use.
    #' @param folds [`integer`]\cr
    #'   Number of folds to use for PipeOpLearnerCV. Defaults to 3.
    #' @template return_auditor
    initialize = function(learner, folds = 3L) {
      self$learner = CVLearnerPredictor$new(learner, folds)
    },
    #' @description
    #' Fit the cross-validated learner and compute correlation
    #'
    #' @template params_data_resid
    #' @template params_mask
    #' @template return_fit
    fit = function(data, resid, mask) {
      l = self$learner$clone()
      h = l$fit_transform(data, resid)
      corr = mean(h*resid)
      return(list(corr, l))
    }
  )
)

#' @describeIn CVLearnerAuditorFitter  Cross-Validated auditor based on rpart
#' @family AuditorFitter
#' @export
CVTreeAuditorFitter = R6::R6Class("CVTreeAuditorFitter",
  inherit = CVLearnerAuditorFitter,
  public = list(
    #' @description
    #' Define a cross-validated AuditorFitter from a rpart learner
    #' See [`mlr3pipelines::PipeOpLearnerCV`] for more information on
    #' cross-validated learners.
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "rpart"))
      super$initialize(learner = lrn("regr.rpart"))
    }
  )
)

#' @describeIn CVLearnerAuditorFitter Cross-Validated auditor based on glmnet
#' @family AuditorFitter
#' @export
CVRidgeAuditorFitter = R6::R6Class("CVRidgeAuditorFitter",
  inherit = CVLearnerAuditorFitter,
  public = list(
    #' @description
    #' Define a cross-validated AuditorFitter from a glmnet learner.
    #' See [`mlr3pipelines::PipeOpLearnerCV`] for more information on
    #' cross-validated learners.
    initialize = function() {
      mlr3misc::require_namespaces(c("mlr3learners", "glmnet"))
      super$initialize(learner = lrn("regr.glmnet", alpha = 0))
    }
  )
)
