# label must be object of Surv
#' @export
MCBoostSurv = R6::R6Class("MCBoostSurv",
  inherit = MCBoost,
  public = list(
    # if time_points = NULL, use timepoints in data
    time_points = NULL,
    time_buckets = NULL,
    bucket_strategies = c("even_splits", "quantiles"), # , "predictions"),#FIXME Darf ich das?
    bucket_aggregation = NULL,
    time_eval = NULL, # like 75% IBS#
    time_points_eval = NULL,
    loss = c("censored_brier", "brier", "censored_brier_proper"),
    # FIXME --- additional parameters? e.g. time buckets?

    initialize = function(
      # time_points = NULL,
      max_iter = 5,
      alpha = 1e-4,
      eta = 1,
      # partition=TRUE,
      num_buckets = 2, # probability_buckets
      time_buckets = 1L,
      time_eval = 1,
      bucket_strategy = "even_splits",
      bucket_aggregation = NULL,
      rebucket = FALSE,
      eval_fulldata = FALSE,
      multiplicative = TRUE,
      auditor_fitter = NULL,
      subpops = NULL,
      default_model_class = LearnerSurvKaplan,
      init_predictor = NULL,
      loss = "censored_brier",
      iter_sampling = "none") {

      super$initialize(
        max_iter,
        alpha,
        eta,
        # partition,
        num_buckets,
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

      self$time_eval = assert_double(time_eval, lower = 0.1, upper = 1, finite = TRUE, len = 1)
      self$time_buckets = assert_int(time_buckets, lower = 1)
      # if (self$time_buckets == 1L && self$partition) stop("If partition=TRUE, num_buckets musst be > 1!")
      self$bucket_aggregation = assert_function(bucket_aggregation, null.ok = TRUE)
      self$loss = assert_choice(loss, choices = c("censored_brier", "brier", "censored_brier_proper"))

    }
  ),
  # FIXME kann man das vielleicht do mit MCBoost zusammenführen ?

  private = list(
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
        update_weights = (self$eta * update_sign * deltas)
        new_preds = orig_preds - update_weights
      }

      if (audit) { # FIXME does this need to change?
        auditor_effects = c(auditor_effects, list(abs(deltas)))
      }

      # also correct for survival property: monotonically decreasing & between 0 and 1
      survival_curve = make_survival_curve(clip_prob(new_preds))
      return(survival_curve)
    },

    compute_residuals = function(prediction, labels) {
      residuals = private$calc_residual_matrix(prediction, labels)
      if (self$loss == "brier") {
        return(as.data.table(as.data.frame(residuals)))
      }

      if (self$loss == "censored_brier" || self$loss == "censored_brier_proper") {

        proper = ifelse(self$loss == "censored_brier", FALSE, TRUE)

        cens_distr = survival::survfit(survival::Surv(unlist(labels[, "time"]), (1 - unlist(labels[, "status"]))) ~ 1)
        cens_matrix = matrix(c(cens_distr$time, cens_distr$surv), ncol = 2)

        # weight the residual matrix according to Graf et.al(1999)
        weighted_residuals = mlr3proba::.c_weight_survival_score(
          residuals,
          labels,
          self$time_points,
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
      mask = outer(as.numeric(unlist(labels[, "time"])), self$time_points, FUN = ">")
      igs[mask] = igs[mask] - 1
      as.matrix(igs)
    },

    assert_labels = function(labels, ...) {
      if (!inherits(labels, "Surv")) {
        if (is.matrix(labels) || is.data.frame(labels)) {
          data = as.data.table(as.data.frame(labels))
        }

        if (is.data.table(labels)) {
          labels = assert_data_table(labels, col.names = "named")
          if (sum(colnames(labels) %in% c("status", "time")) < 2) stop("labels must have the names status and time")
          labels = survival::Surv(unlist(labels[, "time"]), unlist(labels[, "status"]))
        }
      }

      labels = mlr3proba::assert_surv(labels)

      private$create_time_points(labels, ...)

      labels
    },


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

        # if there are more time points in prediction, just add them.

        add_time_points = diff %in% num_colnames

        if (length(add_time_points)) {

          self$time_points = sort(unique(c(self$time_points, diff[add_time_points])))

          diff <- diff[!add_time_points]
        }

        if (length(diff)) warning(paste0("Your input time_points have more points than the predicted time_points:", diff, ". The input time_points or extracted frome the labels are not used."))

      }

      # There are different time_points in the predicted probabilities
      # & the columns of predicted probabilities not have names
      if (is.null(num_colnames) && (!length(diff) || length(num_colnames) != length(time_points))) {
        stop("Predicted values do not have columnnames and do not match the time_points (input) or labels")
      }


      # There are no columnnames, but the length matches
      if (is.null(num_colnames) && length(num_colnames) == length(time_points)) {
        colnames(probs) = as.character(time_points)
      }

      self$time_points = assert_numeric(self$time_points,
        sorted = TRUE, unique = TRUE,
        min.len = 1, any.missing = FALSE,
        lower = 0, null.ok = TRUE)


      # set time_points_eval
      if (self$time_eval < 1) {
        self$time_points_eval = self$time_points[self$time_points < max(self$time_points[is.finite(self$time_points)]) * self$time_eval]
      } else {
        self$time_points_eval = self$time_points
      }

      return(probs)
    },



    # FIXME
    create_buckets = function(pred_probs) {

      if (self$time_eval < 1) {
        max_time = max(self$time_points)
      } else {
        max_time = Inf
      }

      min_time = min(self$time_points[is.finite(self$time_points)])

      if (self$time_eval == 1L) {
        buckets = list(ProbRange2D$new())
      } else {
        buckets = list(ProbRange2D$new(time = ProbRange$new(lower = -Inf, upper = max_time)))
      }

      if (self$time_buckets == 1 && self$num_buckets == 1) {
        return(buckets)
      }

      if (self$time_buckets > 1L) {

        if (self$bucket_strategy == "even_splits") {
          time_parts = even_bucket(
            c(0, seq_len(self$time_buckets)),
            self$time_buckets, min_time, max_time)
        } else { # if (self$bucket_strategy == "quantiles") {
          time_parts = quantile(self$time_points, seq(0, 1, length.out = self$time_buckets + 1))
        }

        time_parts[[1]] = -Inf
        time_parts[[length(time_parts)]] = max_time

      } else {
        time_parts = list(-Inf, max_time)
      }

      for (i in seq_len(self$time_buckets)) {

        time_range = ProbRange$new(lower = time_parts[[i]], upper = time_parts[[i + 1]])
        in_time = time_range$in_range_mask(self$time_points)

        # skip if there is no probability in time frame
        if (!any(in_time)) {
          next
        }

        prob_in_time = unlist(pred_probs[, in_time, with = FALSE])

        if (!is.null(self$bucket_aggregation)) {
          prob_in_time = apply(as.matrix(prob_in_time), 1, self$bucket_aggregation)
        }


        if (self$bucket_strategy == "even_splits" && self$num_buckets > 1) {
          prob_parts = even_bucket(
            c(0, seq_len(self$num_buckets)),
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




    get_masked = function(data, resid, idx, probs, bucket) {
      # fixme what happens with only one time point
      # FIXME geht das?
      # if (sum(mask) < 1L) next # case no obs. are in the bucket. Are assigned corrs=0.

      mask = bucket$in_range_mask(probs[idx, ])

      if (is.null(mask)) {
        return(NULL)
      }

      data_m = data[idx, ][mask$n, ]
      idx_m = idx[mask$n]
      resid_m = resid[idx, ][mask$n, mask$time, with = FALSE]

      if (is.null(self$bucket_aggregation)) {
        resid_m [!mask$matrix] = 0
      }

      # IBS
      if (test_data_frame(resid_m, min.cols = 2) | testArray(resid_m, min.d = 2)) {
        resid_m = rowMeans(resid_m)
      }

      return(list(data_m = data_m, resid_m = resid_m, idx_m = idx_m))
    },

    calculate_corr = function(auditor, data, resid, idx) {
      mean(auditor$predict(data[idx, ]) * rowMeans(resid[idx, ]))
    }

  )
)
