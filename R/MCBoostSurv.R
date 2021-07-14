MCBoostSurv = R6::R6Class("MCBoostSurv",
  inherit = MCBoost,
  public = list(
    time_points = NULL, 
    #FIXME --- additional parameters? e.g. time buckets? 
    initialize = function(
      time_points,
      max_iter=5,
      alpha=1e-4,
      eta=1,
      partition=TRUE,
      num_buckets=2,
      bucket_strategy="simple",
      rebucket=FALSE,
      multiplicative=TRUE,
      auditor_fitter=NULL,
      subpops=NULL,
      default_model_class=ConstantPredictor, #FIXME must be constant over time
      init_predictor=NULL,
      iter_sampling="none") {
      
      #FIXME does it make sense to include time_points here already? Preprocess? Checks? 
      
      self$time_points = time_points
      
      super$initialize(
        max_iter,
        alpha,
        eta,
        partition,
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
    }
  ),
  private = list(
    update_probs = function(orig_preds, model, x, mask = NULL, audit = FALSE, ...) {

      if (self$multiplicative) {
        update_weights = exp(- self$eta * deltas)
      } else {
        update_weights = (self$eta * deltas)
      }

      new_preds = private$calc_new_preds(orig_preds, update_weights)

      
      if (audit) {
        self$auditor_effects = c(self$auditor_effects, list(abs(deltas)))
      }
      return(clip_prob(new_preds))
    },
    
    calc_new_preds = function(orig_preds, update_weights){
      if (self$multiplicative) {
        # multiply weight by row wise + add noise when 0 
        pmax(orig_preds, 1e-4) * update_weights
        #sweep(pmax(orig_preds, 1e-4), 1, update_weights, "*"))
      } else {
        clip_prob(orig_preds - update_weights)
      }
    },

    compute_residuals = function(prediction, labels) {
      weighted_score_matrix =  private$calc_weighted_residuals(prediction, labels)
      rowMeans(weighted_score_matrix)
    }, 
    
    
    #FIXME should be proper & eps a hyperparameter?
    calc_weighted_residuals = function (prediction, 
                                        labels,
                                        proper = FALSE, 
                                        eps = 1e-4){
      
      
      if (is.null(self$time_points) || !length(self$time_points)) {
        unique_times = unique(sort(labels[, "time"]))
      } else {
        #FIXME use of internal method of mlr3proba
        unique_times = mlr3proba:::c_get_unique_times(labels[, "time"], self$time_points) 
      }
      
      residuals = private$calc_residual_matrix_r(prediction, labels, unique_times)
      
      cens_distr = survival::survfit(survival::Surv(labels[, "time"], (1 - labels[, "status"])) ~ 1)
      cens_matrix = matrix(c(cens_distr$time, cens_distr$surv), ncol = 2)
      
      #FIXME use of internal method of mlr3proba
      
      # weight the residual matrix according to Graf et.al(1999)
      weighted_residuals = mlr3proba:::c_weight_survival_score(residuals, 
                                                               labels, 
                                                               unique_times,
                                                               cens_matrix,
                                                               proper, eps)
      
      weighted_residuals = as.data.frame(weighted_residuals)
      
      colnames(weighted_residuals) = unique_times
      
      weighted_residuals
    }, 
    
    
    #calculate for every time step and every survival curve the residual
    calc_residual_matrix_r = function (prediction, labels, unique_times){
      nr_obs = length(labels)
      nc_times = length(unique_times)
      labels_num = as.numeric(labels)
      
      igs = matrix (nrow = nr_obs, ncol = nc_times)
      
      for (i in seq_len(nr_obs)) {
        for (j in seq_len(nc_times)) {
          if(labels_num[i] > unique_times[j]) {
            igs[i, j] = (prediction[i, j] - 1)
          } else {
            igs[i, j] = (prediction[i, j] - 0)
          }
        }
      }
      
      igs
    }, 
    
    check_labels =  function(labels){
      #FIXME # somthing missing
      #FIXME use of internal method of mlr3proba
      mlr3proba:::assert_surv(labels)
    }, 
    
    create_buckets = function(pred_probs) {
      #FIXME
      # maybe als buckets throuh time?  
      probs = private$get_probs(pred_probs, new_probs)
      
      buckets = list(ProbRange$new())
      
      if (self$partition && self$num_buckets > 1L) {
        quantiles = quantile(probs, seq(0,1,length.out=self$num_buckets + 1))
        
        buckets = c(buckets, 
                    lapply(seq_len(self$num_buckets), 
                           function(b) {
                             ProbRange$new(quantiles[b], quantiles[b+1])
                           }))
        buckets[[2]]$lower = -Inf
        buckets[[length(buckets)]]$upper = Inf
      } else {
        #FIXME Kann wa nicht auch sein, dass partition hier FALSE ist?
        if (self$num_buckets == 1L) stop("If partition=TRUE, num_buckets musst be > 1!")
      }
      
      buckets
    }, 
    
    assert_prob = function(probs, data) {
      #FIXME
      # needs to be adapted!!
      
      
      # distr6::assertDistribution(prob)
      # checkmate::assertTRUE(length(prob) == nrow(data))

      
      if (inherits(probs,"Distribution"))
        probs = t(as.matrix(probs$survival(self$time_points)))
        colnames(probs) = self$time_points
        
      #FIXME insert check 
      
      probs
    },
    
    get_probs = function(pred_probs, new_probs){
      #FIXME do not always calculate again? Does it make sense to take mean?  
      if (self$rebucket) {
        probs = rowMeans(new_probs)
      } else {
        probs = rowMeans(pred_probs)
      }
    }
    
  )
)
