# Check if several time_points?
# To-Do:

MCBoostSurv = R6::R6Class("MCBoostSurv",
  inherit = MCBoost,
  public = list(
    # if time_points = NULL, use timepoints in data
    time_points = NULL,
    time_buckets = NULL,
    bucket_strategies = c("even_splits", "quantiles"), # , "predictions"),#FIXME Darf ich das?
    bucket_aggregation = NULL,
    # FIXME --- additional parameters? e.g. time buckets?

    initialize = function(
      time_points = NULL,
      max_iter = 5,
      alpha = 1e-4,
      eta = 1,
      # partition=TRUE,
      num_buckets = 2, # probability_buckets
      time_buckets = 1L,
      bucket_strategy = "even_splits",
      bucket_aggregation = NULL,
      rebucket = FALSE,
      multiplicative = TRUE,
      auditor_fitter = NULL,
      subpops = NULL,
      default_model_class = ConstantPredictor, # FIXME must be constant over time
      init_predictor = NULL,
      iter_sampling = "none") {

      super$initialize(
        max_iter,
        alpha,
        eta,
        # partition,
        num_buckets,
        bucket_strategy,
        rebucket,
        multiplicative,
        auditor_fitter,
        subpops,
        default_model_class,
        init_predictor,
        iter_sampling
      )

      # FIXME does it make sense to include time_points here already? Preprocess? Checks?
      self$time_points = assert_numeric(time_points,
        sorted = TRUE, unique = TRUE,
        min.len = 1, any.missing = FALSE,
        lower = 0, null.ok = TRUE)

      self$time_buckets = assert_int(time_buckets, lower = 1)
      # if (self$time_buckets == 1L && self$partition) stop("If partition=TRUE, num_buckets musst be > 1!")
      self$bucket_aggregation = assert_function(bucket_aggregation, null.ok = TRUE)
    }
  ),
  # FIXME kann man das vielleicht do mit MCBoost zusammenführen ?

  private = list(
    update_probs = function(orig_preds, model, x, mask = NULL, audit = FALSE, ...) {

      dim_probs = dim(orig_preds)
      deltas = matrix(0, nrow = dim_probs[1], ncol = dim_probs[2])

      deltas <<- deltas
      deltas_small <<- deltas[mask$n, mask$time]
      n_1 <<- mask$n
      time_1 <<- mask$time
      matrix_1 <<- mask$matrix
      mask_1 = mask

      if (!is.null(self$bucket_aggregation)) {
        deltas[mask$n, mask$time] = (1 * (mask$matrix))
      } else {
        deltas[mask$n, mask$time] = 1
      }

      auditor_predictions = model$predict(x, ...)
      deltas = deltas * auditor_predictions

      if (self$multiplicative) {
        update_weights = exp(-self$eta * deltas)
        new_preds = pmax(orig_preds, 1e-4) * update_weights
      } else {
        update_weights = (self$eta * deltas)
        new_preds = clip_prob(orig_preds - update_weights)
      }

      if (audit) { # FIXME does this need to change?
        auditor_effects = c(auditor_effects, list(abs(deltas)))
      }

      return(clip_prob(new_preds)) # FIXME also check for survival property?
    },


    compute_residuals = function(prediction, labels) {

      proper = FALSE
      eps = 1e-4

      residuals = private$calc_residual_matrix_r(prediction, labels)

      cens_distr = survival::survfit(survival::Surv(labels[, "time"], (1 - labels[, "status"])) ~ 1)
      cens_matrix = matrix(c(cens_distr$time, cens_distr$surv), ncol = 2)

      # weight the residual matrix according to Graf et.al(1999)
      weighted_residuals = mlr3proba::.c_weight_survival_score(
        residuals,
        labels,
        self$time_points,
        cens_matrix,
        proper, 
        eps)

      weighted_residuals = as.data.frame(weighted_residuals)

      colnames(weighted_residuals) = self$time_points

      weighted_residuals
    },


    # calculate for every time step and every survival curve the residual
    calc_residual_matrix_r = function(prediction, labels) {
      nr_obs = length(labels)
      nc_times = length(self$time_points)
      labels_num = as.numeric(labels)

      igs = matrix(nrow = nr_obs, ncol = nc_times)

      for (i in seq_len(nr_obs)) {
        for (j in seq_len(nc_times)) {
          if (labels_num[i] > self$time_points[j]) {
            igs[i, j] = (prediction[i, j] - 1)
          } else {
            igs[i, j] = (prediction[i, j] - 0)
          }
        }
      }

      igs
    },

    check_labels = function(labels) {
      # FIXME # somthing missing? e.g. other formats
      labels = mlr3proba::assert_surv(labels)

      # FIXME woanders hin
      if (is.null(self$time_points) || !length(self$time_points)) {
        self$time_points = unique(sort(labels[, "time"]))
      } else {
        # FIXME use of internal method of mlr3proba
        self$time_points = mlr3proba::.c_get_unique_times(labels[, "time"], self$time_points)
      }

      labels
    },

    # FIXME
    create_buckets = function(pred_probs) {

      buckets = list(ProbRange2D$new())

      if (self$time_buckets == 1 && self$num_buckets == 1) {
        return(buckets)
      }

      if (self$time_buckets > 1L) {

        if (self$bucket_strategy == "even_splits") {
          time_parts = even_bucket(
            c(0, seq_len(self$time_buckets)),
            self$time_buckets, min(self$time_points[!is.infinite(self$time_points)]),
            max(self$time_points[!is.infinite(self$time_points)]))
        } else { # if (self$bucket_strategy == "quantiles") {
          time_parts = quantile(self$time_points, seq(0, 1, length.out = self$time_buckets + 1))
        }

        time_parts[[1]] = -Inf
        time_parts[[length(time_parts)]] = Inf

      } else {
        time_parts = list(-Inf, Inf)

      }

      for (i in seq_len(self$time_buckets)) {

        time_range = ProbRange$new(lower = time_parts[[i]], upper = time_parts[[i + 1]])
        in_time = time_range$in_range_mask(self$time_points)

        # skip if there is no probability in time frame
        if (any(in_time)) {
          prob_in_time = pred_probs[, in_time]
        } else {
          print(length(buckets))
          prob_in_time = pred_probs # FIXME Was passiert wenn Zeitpunkt nicht definiert?
        }

        if (!is.null(self$bucket_aggregation)) {
          prob_in_time = apply(as.matrix(prob_in_time), 1, self$bucket_aggregation)
        }

        if (self$bucket_strategy == "even_splits") {
          prob_parts = even_bucket(
            c(0, seq_len(self$num_buckets)),
            self$num_buckets, min(prob_in_time[!is.infinite(prob_in_time)]),
            max(prob_in_time[!is.infinite(prob_in_time)]))
        }

        if (self$bucket_strategy == "quantiles") {
          prob_parts = quantile(prob_in_time, seq(0, 1, length.out = self$num_buckets + 1))
        }

        prob_range = c(mlr3misc::map(seq_len(self$num_buckets), function(b) {
          ProbRange$new(prob_parts[b], prob_parts[b + 1])
        }))

        # FIXME --> Was passiert, wenn hier ein keine Wahrscheinlichkeit in der prob_part ist?

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

    assert_prob = function(probs, data) {
      # FIXME
      # needs to be adapted!!


      # distr6::assertDistribution(prob)
      # checkmate::assertTRUE(length(prob) == nrow(data))

      if (inherits(probs, "Distribution")) {
        probs = t(as.matrix(probs$survival(self$time_points)))
      }

      colnames(probs) = self$time_points
      # FIXME insert check

      probs
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
      resid_m = resid[idx, ][mask$n, mask$time]
      
      if (is.null(self$bucket_aggregation)) {
        resid_m = resid_m * as.numeric(mask$matrix)
      }

      # IBS
      resid_m = rowMeans(resid_m)

      return(list(data_m = data_m, resid_m = resid_m, idx_m = idx_m))
    }
  )
)
