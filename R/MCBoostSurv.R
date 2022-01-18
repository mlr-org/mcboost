#' Multi-Calibration Boosting
#'
#' @description
#'   Implements Multi-Calibration Boosting by Hebert-Johnson et al. (2018) and
#'   Multi-Accuracy Boosting by Kim et al. (2019) for the multi-calibration of a
#'   machine learning model's prediction for survival models.
#'   Multi-Calibration works best in scenarios where the underlying data & labels are unbiased
#'   but a bias is introduced within the algorithm's fitting procedure. This is often the case,
#'   e.g. when an algorithm fits a majority population while ignoring or under-fitting minority
#'   populations.\cr
#'   Expects initial models that predict probobilities (between 0 and 1) for different time points.
#'   The method defaults to `Multi-Accuracy Boosting` as described in Kim et al. (2019).
#'   In order to obtain behaviour as described in Hebert-Johnson et al. (2018) set
#'   `multiplicative=FALSE` and `num_buckets` to 10.
#'   \itemize{
#'   For additional details, please refer to the relevant publications:
#'     \item{Hebert-Johnson et al., 2018. Multicalibration: Calibration for the (Computationally-Identifiable) Masses.
#'      Proceedings of the 35th International Conference on Machine Learning, PMLR 80:1939-1948.
#'      https://proceedings.mlr.press/v80/hebert-johnson18a.html.}{}
#'     \item{Kim et al., 2019. Multiaccuracy: Black-Box Post-Processing for Fairness in Classification.
#'     Proceedings of the 2019 AAAI/ACM Conference on AI, Ethics, and Society (AIES '19).
#'     Association for Computing Machinery, New York, NY, USA, 247–254.
#'     https://dl.acm.org/doi/10.1145/3306618.3314287}{}
#'  }
#' @export
MCBoostSurv = R6::R6Class("MCBoostSurv",
  inherit = MCBoost,
  public = list(

    #' @field max_iter [`integer`] \cr
    #'   The maximum number of iterations of the multi-calibration/multi-accuracy method.
    max_iter = NULL,

    #' @field alpha  [`numeric`] \cr
    #'   Accuracy parameter that determines the stopping condition.
    alpha = NULL,

    #' @field eta  [`numeric`] \cr
    #'   Parameter for multiplicative weight update (step size).
    eta = NULL,

    #' @field num_buckets [`integer`] \cr
    #' The number of buckets to split into in addition to using the whole sample.
    num_buckets = NULL,

    #' @field bucket_strategy  [`character`] \cr
    #'   Currently only supports "simple", even split along probabilities.
    #'   Only relevant for `num_buckets` > 1.
    bucket_strategy = NULL,

    #' @field rebucket [`logical`] \cr
    #'   Should buckets be re-calculated at each iteration?
    rebucket = NULL,

    #' @field eval_fulldata [`logical`] \cr
    #'   Should auditor be evaluated on the full data?
    eval_fulldata = NULL,

    #' @field partition [`logical`] \cr
    #'   True/False flag for whether to split up predictions by their "partition"
    #'   (e.g., predictions less than 0.5 and predictions greater than 0.5).
    partition = NULL,

    #' @field multiplicative [`logical`] \cr
    #'   Specifies the strategy for updating the weights (multiplicative weight vs additive).
    multiplicative = NULL,

    #' @field iter_sampling [`character`] \cr
    #'   Specifies the strategy to sample the validation data for each iteration.
    iter_sampling = NULL,

    #' @field auditor_fitter [`AuditorFitter`] \cr
    #'   Specifies the type of model used to fit the residuals.
    auditor_fitter = NULL,

    #' @field predictor [`function`] \cr
    #'   Initial predictor function.
    predictor = NULL,

    #' @field iter_models [`list`] \cr
    #'   Cumulative list of fitted models.
    iter_models = list(),

    #' @field iter_partitions [`list`] \cr
    #'   Cumulative list of data partitions for models.
    iter_partitions = list(),

    #' @field iter_corr [`list`] \cr
    #'   Auditor correlation in each iteration.
    iter_corr = list(),

    #' @field auditor_effects [`list`] \cr
    #'   Auditor effect in each iteration.
    auditor_effects = list(),

    #' @field time_points [`integer`] \cr
    #'   Times included in the prediction (columnames)
    time_points = NULL,

    #' @field time_buckets [`integer`] \cr
    #'   The number of buckets to split the time points (columns) of the prediction.
    time_buckets = NULL,

    #' @field bucket_strategies [`character`] \cr
    #'   Possible bucket_strategies in McBoostSurv.
    #'   Only relevant for `time_buckets` > 1.
    #'   `even_splits`: split buckets evenly
    #'   `quantiles`: split buckets by quantiles
    bucket_strategies = c("even_splits", "quantiles"),

    #' @field bucket_aggregation [`function`] \cr
    #'   If not NULL, predictions are not selected by time/probability,
    #'   but by time/individual. Individuals are selected by aggregated value per
    #'   individual (e.g. mean).
    #'   Only relevant for `time_buckets` > 1.
    bucket_aggregation = NULL,

    #' @field max_time_quantile [`double`] \cr
    #' Time quantile which should be evaluated and multicalibrated.
    #' Similar to a 75%-Integrated Brier Score.
    max_time_quantile = NULL,

    #' @field time_points_eval [`integer`] \cr
    #' Vector of time_points that should be evaluated.
    time_points_eval = NULL,

    #' @field loss [`character`] \cr
    #' Loss function which is optimized during boosting.
    #'  `censored_brier`: censored version of the integrated brier score
    #'  `brier`: uncensored version of the integrated brier score
    #'  `censored_brier_proper`: proper version of the censored version of the integrated brier score
    #'  For more details, we are referring to https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.html.
    loss = c("censored_brier", "brier", "censored_brier_proper"),
    #' @description
    #'   Initialize a multi-calibration instance.
    #'
    #' @param max_iter [`integer`] \cr
    #'   The maximum number of iterations of the multi-calibration/multi-accuracy method.
    #'   Default `5L`.
    #' @param alpha  [`numeric`] \cr
    #'   Accuracy parameter that determines the stopping condition. Default `1e-4`.
    #' @param eta  [`numeric`] \cr
    #'   Parameter for multiplicative weight update (step size). Default `1.0`.
    #' @param partition [`logical`] \cr
    #'   True/False flag for whether to split up predictions by their "partition"
    #'   (e.g., predictions less than 0.5 and predictions greater than 0.5).
    #'   Defaults to `TRUE` (multi-accuracy boosting).
    #' @param num_buckets [`integer`] \cr
    #'   The number of buckets to split into in addition to using the whole sample. Default `2L`.
    #' @param rebucket [`logical`] \cr
    #'   Should buckets be re-done at each iteration? Default `FALSE`.
    #' @param eval_fulldata [`logical`] \cr
    #'   Should the auditor be evaluated on the full data or on the respective bucket for determining
    #'   the stopping criterion? Default `FALSE`, auditor is only evaluated on the bucket.
    #'   This setting keeps the implementation closer to the Algorithm proposed in the corresponding
    #'   multi-accuracy paper (Kim et al., 2019) where auditor effects are computed across the full
    #'   sample (i.e. eval_fulldata = TRUE).
    #' @param multiplicative [`logical`] \cr
    #'   Specifies the strategy for updating the weights (multiplicative weight vs additive).
    #'   Defaults to `TRUE` (multi-accuracy boosting). Set to `FALSE` for multi-calibration.
    #' @param auditor_fitter [`AuditorFitter`]|[`character`]|[`mlr3::Learner`] \cr
    #'   Specifies the type of model used to fit the
    #'   residuals. The default is [`RidgeAuditorFitter`].
    #'   Can be a `character`, the name of a [`AuditorFitter`], a [`mlr3::Learner`] that is then
    #'   auto-converted into a [`LearnerAuditorFitter`] or a custom [`AuditorFitter`].
    #' @template params_subpops
    #' @param default_model_class `Predictor` \cr
    #'   The class of the model that should be used as the init predictor model if
    #'   `init_predictor` is not specified. Defaults to `ConstantPredictor` which
    #'   predicts a constant value.
    #' @param init_predictor [`function`]|[`mlr3::Learner`] \cr
    #'   The initial predictor function to use (i.e., if the user has a pretrained model).
    #'   If a `mlr3` `Learner` is passed, it will be autoconverted using `mlr3_init_predictor`.
    #'   This requires the [`mlr3::Learner`] to be trained.
    #' @param iter_sampling [`character`] \cr
    #'   How to sample the validation data for each iteration?
    #'   Can be `bootstrap`, `split` or `none`.\cr
    #'   "split" splits the data into `max_iter` parts and validates on each sample in each iteration.\cr
    #'   "bootstrap" uses a new bootstrap sample in each iteration.\cr
    #'   "none" uses the same dataset in each iteration.
    #' @param time_buckets [`integer`] \cr
    #'  The number of buckets to split the time points (columns) of the prediction.
    #' @param bucket_strategy [`character`] \cr
    #' Bucketstragy for bucketing.
    #'   `even_splits`: split buckets evenly
    #'   `quantiles`: split buckets by quantiles
    #' @param max_time_quantile [`double`] \cr
    #'   Time quantile which should be evaluated and multicalibrated.
    #'   Can be used to perform multi-calibration only up to the `max_time_quantile` percent of timepoints.
    #'   Initialized to `1`.
    #' @param bucket_aggregation [`function`] \cr
    #'   If not NULL, predictions are not selected by time/probability,
    #'   but by time/individual. Individuals are selected by aggregated value per
    #'   individual (e.g. mean).
    #'   Only relevant for `time_buckets` > 1.
    #' @param loss [`character`] \cr
    #'      #' Loss function which is optimized during boosting.
    #'  `censored_brier`: censored version of the integrated brier score
    #'  `brier`: uncensored version of the integrated brier score
    #'  `censored_brier_proper`: proper version of the censored version of the integrated brier score
    #'  For more details, we are referring to https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.html.
    initialize = function(
      max_iter = 25,
      alpha = 1e-4,
      eta = 0.1,
      num_buckets = 1, # probability_buckets
      partition =  ifelse(num_buckets>1, TRUE, FALSE),
      time_buckets = 2L,
      max_time_quantile = 1,
      bucket_strategy = "even_splits",
      bucket_aggregation = NULL,
      rebucket = FALSE,
      eval_fulldata = FALSE,
      multiplicative = TRUE,
      auditor_fitter = "RidgeAuditorFitter",
      subpops = NULL,
      default_model_class = LearnerSurvKaplan,
      init_predictor = NULL,
      loss = "censored_brier",
      iter_sampling = "none") {

      mlr3misc::require_namespaces("mlr3proba")
      mlr3misc::require_namespaces("survival")
      
      super$initialize(
        max_iter,
        alpha,
        eta,
        num_buckets,
        partition,
        bucket_strategy,
        rebucket,
        eval_fulldata,
        multiplicative,
        auditor_fitter,
        subpops,
        default_model_class,
        init_predictor,
        iter_sampling
      )
      self$max_time_quantile = assert_double(max_time_quantile, lower = 0.1, upper = 1, finite = TRUE, len = 1)
      self$time_buckets = assert_int(time_buckets, lower = 1)
      self$bucket_aggregation = assert_function(bucket_aggregation, null.ok = TRUE)
      self$loss = assert_choice(loss, choices = c("censored_brier", "brier", "censored_brier_proper"))
    }
  ),

  private = list(
    # update probabilities according to base learner "model" for every x and t in a bucket
    update_probs = function(orig_preds, model, x, mask = NULL, update_sign = 1, audit = FALSE, ...) {

      dim_probs = dim(orig_preds)
      deltas = matrix(0, nrow = dim_probs[1], ncol = dim_probs[2])

      if (!is.null(self$bucket_aggregation)) {
        deltas[mask$n, mask$time] = (1 * (mask$matrix))
      } else {
        deltas[mask$n, mask$time] = 1
      }

      auditor_predictions = model$predict(x, ...)
      deltas = deltas * auditor_predictions

      if (self$multiplicative) {

        update_weights = exp(-self$eta * update_sign * deltas)

        # Add a small term to enable moving away from 0.
        new_preds = update_weights * pmax(orig_preds, 1e-4)

      } else {
        # additive update
        update_weights = (self$eta * update_sign * deltas)
        new_preds = orig_preds - update_weights
      }

      if (audit) {
        self$auditor_effects = c(self$auditor_effects, list(abs(deltas)))
      }

      # also correct for survival property: monotonically decreasing & between 0 and 1
      survival_curve = make_survival_curve(clip_prob(new_preds))

      return(survival_curve)
    },

    # compute matrix of pseudo-residuals according to selected loss function
    compute_residuals = function(prediction, labels) {
      residuals = private$calc_residual_matrix(prediction, labels)
      if (self$loss == "brier") {
        return(as.data.table(as.data.frame(residuals)))
      }

      if (self$loss == "censored_brier" || self$loss == "censored_brier_proper") {

        proper = ifelse(self$loss == "censored_brier", FALSE, TRUE)

        surv = invoke(survival::Surv, unlist(labels[, "time"]), (1 - unlist(labels[, "status"])))
        cens_distr = invoke(survival::survfit, surv ~ 1)
        cens_matrix = matrix(c(cens_distr$time, cens_distr$surv), ncol = 2)

        # weight the residual matrix according to Graf et.al(1999)
        weighted_residuals = mlr3proba::.c_weight_survival_score(
          residuals,
          labels,
          self$time_points_eval,
          cens_matrix,
          proper,
          eps = 1e-4)

        weighted_residuals = as.data.table(as.data.frame(weighted_residuals))
        colnames(weighted_residuals) = as.character(self$time_points)

        return(weighted_residuals)
      }
    },


    # calculate for every time step and every survival curve the residual
    calc_residual_matrix = function(prediction, labels) {
      igs = as.matrix(prediction)
      mask = outer(as.numeric(unlist(labels[, "time"])), self$time_points_eval, FUN = ">")
      igs[mask] = igs[mask] - 1
      as.matrix(igs)
    },


    # check labels
    assert_labels = function(labels, ...) {

      # check if labels are obejct of class Surv, if not create it
      if (!inherits(labels, "Surv")) {
        if (is.matrix(labels) || is.data.frame(labels)) {
          data = as.data.table(as.data.frame(labels))
        }

        if (is.data.table(labels)) {
          labels = assert_data_table(labels, col.names = "named")
          if (sum(colnames(labels) %in% c("status", "time")) < 2) stop("labels must have the names status and time") # nocov
          labels = invoke(survival::Surv, unlist(labels[, "time"]), unlist(labels[, "status"]))
        }
      }

      labels = mlr3proba::assert_surv(labels)

      private$create_time_points(labels, ...)

      labels
    },

    # create time points which are evaluated
    create_time_points = function(labels, ...) {

      dots = list(...)

      input_times = dots$time_points

      if (!is.null(input_times)) {
        self$time_points = input_times
      }

      if (is.null(self$time_points) || !length(self$time_points)) {
        self$time_points = unique(sort(unlist(labels[, "time"])))
      } else {
        self$time_points = mlr3proba::.c_get_unique_times(unlist(labels[, "time"]), self$time_points)
      }


      self$time_points = assert_numeric(self$time_points,
        sorted = TRUE, unique = TRUE,
        min.len = 1, any.missing = FALSE,
        lower = 0, null.ok = TRUE)

    },


    # check input probabilities
    assert_prob = function(probs, data, ...) {

      if (inherits(probs, "PredictionSurv")) {
        probs = as.data.table(probs)$distr[[1]][[1]]
      }

      if (inherits(probs, "Distribution")) {
        probs = t(as.matrix(probs$survival(self$time_points)))
      }

      probs = assert_numeric(as.matrix(probs), lower = 0, upper = 1, null.ok = FALSE, any.missing = FALSE, finite = TRUE)
      probs = assert_data_table(as.data.table(as.data.frame(probs)), types = c("numeric"), col.names = "named", min.cols = 1, min.rows = 1, any.missing = FALSE, null.ok = FALSE)

      num_colnames = as.numeric(colnames(probs))
      diff = setdiff(num_colnames, self$time_points)
      # There are different time_points in the predicted probabilities
      # & the columns of predicted probabilities have names
      # Time points might not have first time_point (0) and "Inf" (or last time_point)
      if (length(diff) && length(num_colnames)) {
        self$time_points = sort(unique(num_colnames))
      }
      diff2 = setdiff(self$time_points, num_colnames)
      if (length(diff2)) warning(paste0("Your input time_points have more points than the predicted time_points.
                                         The input time_points or extracted frome the labels are not used."))

      # There are different time_points in the predicted probabilities
      # & the columns of predicted probabilities not have names
      if (is.null(num_colnames) && (!length(diff) || length(num_colnames) != length(self$time_points))) {
        stop("Predicted values do not have columnnames and do not match the time_points (input) or labels")
      }


      # There are no columnnames, but the length matches
      if (is.null(num_colnames) && length(num_colnames) == length(self$time_points)) {
        colnames(probs) = as.character(self$time_points)
      }

      self$time_points = assert_numeric(self$time_points,
        sorted = TRUE, unique = TRUE,
        min.len = 1, any.missing = FALSE,
        lower = 0, null.ok = TRUE)


      # set time_points_eval
      if (self$max_time_quantile < 1) {
        q = quantile(self$time_points[is.finite(self$time_points)], self$max_time_quantile)
        self$time_points_eval = unique(self$time_points[self$time_points < q])
      } else {
        self$time_points_eval = self$time_points
      }

      return(probs)
    },


    # create buckets according to bucket strategy
    # in every time bucket we create probability buckets based on minimal and maximal
    # survival probabilities within the bucket
    create_buckets = function(pred_probs) {

      # cut the times which are evaluated
      if (self$max_time_quantile < 1) {
        max_time = max(self$time_points_eval)
      } else {
        max_time = Inf
      }

      min_time = min(self$time_points_eval[is.finite(self$time_points_eval)])

      # create time buckets
      if (self$max_time_quantile == 1L) {
        buckets = list(ProbRange2D$new())
      } else {
        buckets = list(ProbRange2D$new(time = ProbRange$new(lower = -Inf, upper = max_time)))
      }

      # no buckets
      if (self$time_buckets == 1 && self$num_buckets == 1) {
        return(buckets)
      }

      # time buckets
      if (self$time_buckets > 1L) {

        if (self$bucket_strategy == "even_splits") {
          time_parts = even_bucket(
            self$time_buckets, min_time, max_time)
        } else { #quantiles
          time_parts = quantile(self$time_points_eval, seq(0, 1, length.out = self$time_buckets + 1))
        }

        time_parts[[1]] = -Inf
        time_parts[[length(time_parts)]] = max_time

      } else {
        time_parts = list(-Inf, max_time)
      }

      # create time buckets
      for (i in seq_len(self$time_buckets)) {

        time_range = ProbRange$new(lower = time_parts[[i]], upper = time_parts[[i + 1]])

        column_time = as.numeric(colnames(pred_probs))

        if(is.null(column_time)){
          column_time = self$time_points
        }

        in_time = time_range$in_range_mask(column_time)

        # skip if there is no probability in time frame
        if (!any(in_time)) {
          next
        }

        # create probability buckets with in the range of the time bucket
        prob_in_time = unlist(pred_probs[, in_time, with = FALSE])


        if (!is.null(self$bucket_aggregation)) {
          prob_in_time = apply(as.matrix(prob_in_time), 1, self$bucket_aggregation)
        }


        if (self$bucket_strategy == "even_splits" && self$num_buckets > 1) {
          prob_parts = even_bucket(
            self$num_buckets, min(prob_in_time[is.finite(prob_in_time)]),
            max(prob_in_time[is.finite(prob_in_time)]))
        }

        if (self$bucket_strategy == "quantiles" && self$num_buckets > 1) {
          prob_parts = quantile(prob_in_time, seq(0, 1, length.out = self$num_buckets + 1))
        }

        if (self$num_buckets == 1) {
          prob_parts = list(0, 1)
        }

        prob_range = c(mlr3misc::map(seq_len(self$num_buckets), function(b) {
          ProbRange$new(prob_parts[[b]], prob_parts[[b + 1]])
        }))

        prob_range[[1]]$lower = -Inf
        prob_range[[length(prob_range)]]$upper = Inf

        buckets = c(buckets, mlr3misc::map(seq_len(self$num_buckets), function(c) {
          ProbRange2D$new(
            prob = prob_range[[c]],
            time = time_range,
            aggregation = self$bucket_aggregation)
        }))
      }

      buckets
    },


    # get values in bucket
    get_masked = function(data, resid, idx, probs, bucket) {

      mask = bucket$in_range_mask(probs[idx, ])

      if (is.null(mask)) {
        return(NULL)
      }

      data_m = data[idx, ][mask$n, ]
      idx_m = idx[mask$n]
      resid_m = resid[idx, ][mask$n, mask$time, with = FALSE]

      if (is.null(self$bucket_aggregation) && !is.null(nrow(mask$matrix))) {
        resid_m [!mask$matrix] = 0
      }

      # IBS
      if (test_data_frame(resid_m, min.cols = 2) || testArray(resid_m, min.d = 2)) {
        resid_m = rowMeans(resid_m)
      }

      return(list(data_m = data_m, resid_m = as.matrix(resid_m), idx_m = idx_m))
    },


    # calculate multicalibration definition / stopping criterion
    calculate_corr = function(auditor, data, resid, idx) {
      mean(auditor$predict(data[idx, ]) * rowMeans(resid[idx, ]))
    }

  )
)
