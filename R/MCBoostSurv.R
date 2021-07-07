
# inherit from MCBoost object and just change private methods

MCBoostSurv = R6::R6Class("MCBoostSurv",
  inherit = MCBoost,
  public = list(
    times = NULL, 
    #FIXME --- additional parameters? e.g. time buckets? 
    initialize = function(
      times,
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
      
      #FIXME does it make sense to include times here already? Preprocess? Checks? 
      
      self$times = times
      
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
      
      deltas = numeric(length(orig_preds))
      deltas[mask] = model$predict(x, ...)[mask]
      
      if (self$multiplicative) {
        update_weights = exp(- self$eta * deltas)
      } else {
        update_weights = -(self$eta * deltas)
      }
      
      new_preds = private$calc_new_preds(orig_preds, update_weights)
      
      if (audit) {
        self$auditor_effects = c(self$auditor_effects, list(abs(deltas)))
      }
      return(new_preds)
    },
    
    calc_new_preds = function(orig_preds, update_weights){
      if (self$multiplicative) {
        distlist = lapply(seq_len(length(orig_preds$ids)), function(x){
          
          model = orig_preds$wrappedModels(orig_preds$ids[[x]])
          distr6::decorate(model , c("CoreStatistics", "ExoticStatistics"))
          
          orig_support = as.numeric(model$properties$support)
          orig_survival = model$survival(orig_support)
          
          #calculate new survival probabilities
          new_survival = clip_prob(pmax(orig_survival, 1e-4)*update_weights[x])
          
          # survival cannot be input, only cdf
          new_cdf = 1- new_survival
          distr6::WeightedDiscrete$new(x=orig_support, cdf = new_cdf)
        })
        
        distr6::VectorDistribution$new(distlist = distlist)
      } else {
        distlist = lapply(seq_len(length(orig_preds$ids)), function(x){
          
          model = orig_preds$wrappedModels(orig_preds$ids[[x]])
          distr6::decorate(model , c("CoreStatistics", "ExoticStatistics"))
          
          
          orig_support = as.numeric(model$properties$support)
          orig_survival = model$survival(orig_support)
          
          #calculate new survival probabilities
          new_survival = clip_prob(orig_survival + update_weights[x])
          
          # survival cannot be input, only cdf
          new_cdf = 1-new_survival
          distr6::WeightedDiscrete$new(x=orig_support, cdf = new_cdf)
        })
        
        distr6::VectorDistribution$new(distlist = distlist)
      }
    },
    
    
    compute_residuals = function(prediction, labels) {
      weighted_score_matrix =  private$calc_weighted_residuals(distribution = prediction, 
                                                               labels = labels)
      rowMeans(weighted_score_matrix)
    }, 
    
    
    
    #FIXME should be proper & eps a hyperparameter?
    calc_weighted_residuals = function (distribution, 
                                        labels,
                                        proper = FALSE, 
                                        eps = 1e-4){
      #FIXME use of internal method of mlr3proba
      mlr3proba:::assert_surv(labels)
      distr6::assertDistribution(distribution)
      
      distr6::decorate(distribution , c("CoreStatistics", "ExoticStatistics"))
      
      
      if (is.null(self$times) || !length(self$times)) {
        unique_times = unique(sort(labels[, "time"]))
      } else {
        #FIXME use of internal method of mlr3proba
        unique_times = mlr3proba:::c_get_unique_times(labels[, "time"], self$times) 
      }
      
      residuals = private$calc_residual_matrix_r(labels, unique_times,
                                                 as.matrix(distribution$survival(unique_times)))
      
      cens_distr = survival::survfit(survival::Surv(labels[, "time"], 1 - labels[, "status"]) ~ 1)
      
      
      #FIXME use of internal method of mlr3proba
      
      # weight the residual matrix according to Graf et.al(1999)
      weighted_residuals = mlr3proba:::c_weight_survival_score(residuals, labels, unique_times,
                                                               matrix(c(cens_distr$time, cens_distr$surv), ncol = 2),
                                                               proper, eps)
      
      weighted_residuals = as.data.frame(weighted_residuals)
      
      colnames(weighted_residuals) = unique_times
      
      weighted_residuals
    }, 
    
    
    #calculate for every time step and every survival curve the residual
    calc_residual_matrix_r = function (labels, unique_times,survival){
      nr_obs = length(labels)
      nc_times = length(unique_times)
      labels_num = as.numeric(labels)
      
      igs = matrix (nrow = nr_obs, ncol = nc_times)
      
      for (i in seq_len(nr_obs)) {
        for (j in seq_len(nc_times)) {
          if(labels_num[i] > unique_times[j]) {
            igs[i, j] = (1 - survival[j, i])
          } else {
            igs[i, j] = (0 - survival[j, i])
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
      probs
    },
    
    get_probs = function(pred_probs, new_probs){
      #FIXME do not always calculate again? 
      if (self$rebucket) {
        probs = sapply(pred_probs$ids, function(x) pred_probs$wrappedModels(x)$mean())
      } else {
        probs = sapply(pred_probs$ids, function(x) pred_probs$wrappedModels(x)$mean())
      }
    }
    
  )
)
