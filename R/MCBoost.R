#' Multi-Accuracy Boosting for R
#'
#' @description
#'   For more details, please refer to https://arxiv.org/pdf/1805.12317.pdf (Kim et al. 2018).
#' @examples
#'   # See vignette for more examples.
#'   # Instantiate the object
#'   mc = MCBoost$new()
#'   # Run multicalibration on training dataset.
#'   mc$multicalibrate(iris[1:100,1:4], factor(sample(c("A","B"), 100, TRUE)))
#'   # Predict on test set
#'   mc$predict_probs(iris[101:150,1:4])
#' @export
MCBoost = R6::R6Class("MCBoost",
  public = list(

    #' @field max_iter [`integer`] \cr
    #'   The maximum number of iterations of the multicalibration/multiaccuracy method.
    max_iter = NULL,

    #' @field alpha  [`numeric`] \cr
    #'   Accuracy parameter that determines the stopping condition.
    alpha = NULL,

    #' @field eta  [`numeric`] \cr
    #'   Parameter for multiplicative weight update (step size).
    eta = NULL,

    #' @field num_buckets [`integer`] \cr
    #' The number of buckets to split into.
    num_buckets = NULL,
    #' @field bucket_strategy  [`character`] \cr
    #'   Currently onle supports "simple", even split along probabilities.
    #'   Only relevant for num_buckets > 1.
    bucket_strategy = NULL,
    #' @field rebucket [`logical`] \cr
    #'   Should buckets be re-done at each iteration?
    rebucket = NULL,
    #' @field partition [`logical`] \cr
    #'   True/False flag for whether to split up predictions by their "partition"
    #'   (e.g., predictions less than
    #'   0.5 and predictions greater than 0.5)
    partition = NULL,

    #' @field multiplicative [`logical`] \cr
    #'   Specifies the strategy for updating the weights (multiplicative weight vs additive).
    multiplicative = NULL,

    #' @field subpop_fitter [`ResidualFitter`] \cr
    #'   Specifies the type of model used to fit the
    #'   residual fxn ('TreeResidualFitter' or 'RidgeResidualFitter' (default)).
    subpop_fitter = NULL,

    #' @field predictor [`function`] \cr
    #'   Initial redictor function.
    predictor = NULL,

    #' @field iter_models [`list`] \cr
    #'   Cummulative list of fitted models.
    iter_models = list(),

    #' @field iter_partitions [`list`] \cr
    #'   Cummulative list of data partitions for models.
    iter_partitions = list(),

    #' @description
    #'   Initialize a multi-calibration instance.
    #'
    #' @param max_iter [`integer`] \cr
    #'   The maximum number of iterations of the multicalibration/multiaccuracy method.
    #' @param alpha  [`numeric`] \cr
    #'   Accuracy parameter that determines the stopping condition.
    #' @param eta  [`numeric`] \cr
    #'   Parameter for multiplicative weight update (step size).
    #' @param partition [`logical`] \cr
    #'   True/False flag for whether to split up predictions by their "partition"
    #'   (e.g., predictions less than
    #'   0.5 and predictions greater than 0.5)
    #' @param num_buckets [`integer`] \cr
    #'   The number of buckets to split into.
    #' @param bucket_strategy  [`character`] \cr
    #'   Currently onle supports "simple", even split along probabilities.
    #'   Only relevant for num_buckets > 1.
    #' @param rebucket [`logical`] \cr
    #'   Should buckets be re-done at each iteration?
    #' @param multiplicative [`logical`] \cr
    #'   Specifies the strategy for updating the weights (multiplicative weight vs additive)
    #' @param subpop_fitter [`ResidualFitter`] \cr
    #'   Specifies the type of model used to fit the
    #'   residual [`TreeResidualFitter`] or [`RidgeResidualFitter`] (default)).
    #' @template params_subpops
    #' @param default_model_class [`Predictor`] \cr
    #'   The class of the model that should be used
    #'   as the MCBoost's default predictor model.
    #' @param init_predictor [`function`] \cr
    #'   The initial predictor function to use (i.e., if
    #'   the user has a pretrained model).
    initialize = function(
                 max_iter=5,
                 alpha=1e-4,
                 eta=1,
                 partition=FALSE,
                 num_buckets=2,
                 bucket_strategy="simple",
                 rebucket=FALSE,
                 multiplicative=FALSE,
                 subpop_fitter=NULL,
                 subpops=NULL,
                 default_model_class=ConstantPredictor,
                 init_predictor=NULL) {
      self$max_iter = assert_int(max_iter)
      self$alpha = assert_number(alpha)
      self$eta = assert_number(eta)
      self$num_buckets = assert_int(num_buckets)
      self$bucket_strategy = assert_string(bucket_strategy, fixed = "simple")
      self$rebucket = assert_flag(rebucket)
      self$partition = assert_flag(partition)
      self$multiplicative = assert_flag(multiplicative)


      # Subpopulations
      if (!is.null(subpops)) {
        self$subpop_fitter = SubpopFitter$new(subpops)
      } else {
        if (is.null(subpop_fitter)) {
          self$subpop_fitter = RidgeResidualFitter$new()
        } else if (inherits(subpop_fitter, "ResidualFitter")) {
          self$subpop_fitter = subpop_fitter
        } else if (subpop_fitter == "TreeResidualFitter") {
          self$subpop_fitter = TreeResidualFitter$new()
        } else {
          self$subpop_fitter = RidgeResidualFitter$new()
        }
      }

      # Initial Predictor
      if (is.null(init_predictor)) {
        # Can be a class-generator -> Instantiate
        if (inherits(default_model_class, "R6ClassGenerator")) {
          dm = default_model_class$new()
        } else {
          dm = assert_class(default_model_class, "Predictor")
        }
        init_predictor = function(data) {dm$predict(data)}
      }
      self$predictor = assert_function(init_predictor, args = "data")
      invisible(self)
  },

  #' @description
  #' Run multicalibration.
  #' @template params_data_label
  multicalibrate = function(data, labels) {

    if (is.matrix(data) || is.data.frame(data)) data = as.data.table(as.data.frame(data))
    assert_data_table(data)
    # data.table to factor
    if (is.data.table(labels) && ncol(labels) == 1L) {
      labels = labels[[1]]
    }
    # factor to one-hot
    if (is.factor(labels)) {
      labels = one_hot(labels)
    }
    assert_numeric(labels, lower = 0, upper = 1)

    pred_probs = assert_numeric(self$predictor(data), len = nrow(data))
    resid = pred_probs - labels

    buckets = list(ProbRange$new())
    if (self$partition && self$num_buckets > 1L) {
      frac = 1 / self$num_buckets
      buckets = c(buckets, mlr3misc::map(seq_len(self$num_buckets), function(b) {
        ProbRange$new((b-1) * frac, b * frac)
      }))
      buckets[[length(buckets)]]$upper = 1.0
    }

    new_probs = pred_probs
    for (i in seq_len(self$max_iter)) {
      corrs = integer(length(buckets))
      models = list()

      if (self$rebucket) {
        probs = new_probs
      } else {
        probs = pred_probs
      }

      # Fit on partitions
      for (j in seq_along(buckets)) {
        mask = buckets[[j]]$in_range_mask(probs)
        data_m = data[mask,]
        resid_m = resid[mask]
        out = self$subpop_fitter$fit_to_resid(data_m, resid_m)
        corrs[j] = out[[1]]
        models = c(models, out[[2]])
      }

      if (abs(max(corrs)) < self$alpha) {
        break
      } else {
        max_key = buckets[[which.max(corrs)]]
        prob_mask = max_key$in_range_mask(probs)
        self$iter_models = c(self$iter_models, models[[which.max(corrs)]])
        self$iter_partitions = c(self$iter_partitions, max_key)
        new_probs = private$update_probs(new_probs, self$iter_models[[length(self$iter_models)]], data, prob_mask)
        resid = new_probs - labels
      }
    }
      invisible(NULL)
    },
    #' @description
    #' Predict a dataset with multi-calibrated predictions
    #' @param x [`data.table`] \cr
    #'   Prediction data.
    #' @param t [`integer`] \cr
    #'   Number of multi-calibration steps to predict. Default: `Inf` (all).
    #' @param ... [`any`] \cr
    #'  Params passed on to other methods. Currently not used.
    #' @return
    #'   Numeric vector of multi-calibrated predictions.
    predict_probs = function(x, t = Inf, ...) {
      if (!length(self$iter_models)) {
        warning("multicalibrate was not run! Returning original predictions!")
      }
      # convert to data.table
      if (is.matrix(x) || is.data.frame(x)) x = as.data.table(as.data.frame(x))
      assert_data_table(x)
      orig_preds = self$predictor(x)
      new_preds = orig_preds
      for (i in seq_along(self$iter_models)) {
        if (i <= t) {
          if (self$rebucket) {
            probs = new_preds
          } else {
            probs = orig_preds
          }
          mask = self$iter_partitions[[i]]$in_range_mask(probs)
          new_preds = private$update_probs(new_preds, self$iter_models[[i]], x, mask=mask, ...)
        }
      }
      return(new_preds)
    }
  ),
  private = list(
    update_probs = function(orig_preds, model, x, mask = NULL, ...) {
      deltas = numeric(length(orig_preds))
      deltas[mask] = model$predict(x, partition_mask = mask, ...)

      if (self$multiplicative) {
        update_weights = exp(- self$eta * deltas)
        new_preds = update_weights * orig_preds
      } else {
        new_preds = orig_preds + deltas
      }
      return(clip_prob(new_preds))
    }
  )
)
