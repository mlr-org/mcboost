# Check if several time_points? 

MCBoostSurv = R6::R6Class("MCBoostSurv",
  inherit = MCBoost,
  public = list(
    time_points = NULL, 
    num_buckets_time = NULL, 
    #FIXME --- additional parameters? e.g. time buckets? 
    initialize = function(
      time_points,
      max_iter=5,
      alpha=1e-4,
      eta=1,
      partition=TRUE,
      num_buckets=2,
      num_buckets_time = 1, 
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

      self$num_buckets_time = num_buckets_time
      
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
  #FIXME kann man das vielleicht do mit MCBoost zusammenführen ? 
  private = list(
    update_probs = function(orig_preds, model, x, mask = NULL, audit = FALSE, ...) {
      
      deltas = numeric(length(orig_preds))
      deltas[mask$mask] = model$predict(x, ...)[mask$mask]

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
      #FIX ME IBS or BS
      
      #rowMeans(weighted_score_matrix)
      
      weighted_score_matrix
    }, 
    
    
    #FIXME should be proper & eps a hyperparameter?
    calc_weighted_residuals = function (prediction, 
                                        labels,
                                        proper = FALSE, 
                                        eps = 1e-4){
      
  
      
      residuals = private$calc_residual_matrix_r(prediction, labels, self$time_points)
      
      cens_distr = survival::survfit(survival::Surv(labels[, "time"], (1 - labels[, "status"])) ~ 1)
      cens_matrix = matrix(c(cens_distr$time, cens_distr$surv), ncol = 2)
      
      #FIXME use of internal method of mlr3proba
      
      # weight the residual matrix according to Graf et.al(1999)
      weighted_residuals = mlr3proba::.c_weight_survival_score(residuals, 
                                                               labels, 
                                                               self$time_points,
                                                               cens_matrix,
                                                               proper, eps)
      
      weighted_residuals = as.data.frame(weighted_residuals)
      
      colnames(weighted_residuals) = self$time_points
      
      weighted_residuals
    }, 
    
    
    #calculate for every time step and every survival curve the residual
    calc_residual_matrix_r = function (prediction, labels, time_points){
      nr_obs = length(labels)
      nc_times = length(time_points)
      labels_num = as.numeric(labels)
      
      igs = matrix (nrow = nr_obs, ncol = nc_times)
      
      for (i in seq_len(nr_obs)) {
        for (j in seq_len(nc_times)) {
          if(labels_num[i] > time_points[j]) {
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
      
      #FIXME woanders hin
      if (is.null(self$time_points) || !length(self$time_points)) {
        self$time_points = unique(sort(labels[, "time"]))
      } else {
        #FIXME use of internal method of mlr3proba
        self$time_points = mlr3proba::.c_get_unique_times(labels[, "time"], self$time_points) 
      }
      
      mlr3proba::assert_surv(labels)
    }, 
    
    create_buckets = function(pred_probs) {
      #FIXME
      # maybe als buckets throuh time?  
      probs = private$get_probs(pred_probs, new_probs)
      
      buckets = list(ProbRange2D$new())
      
      if (self$partition&& (self$num_buckets > 1L || self$num_buckets_time > 1L)) {
        #FIXME
        #if(self$num_buckets > 1L){
          quantiles = quantile(probs, seq(0,1,length.out=self$num_buckets + 1))
        #} else {
          #quantiles = c(-Inf, Inf)
        #}
          
        
        #if(self$num_buckets_time > 1L){
          quantiles_time = quantile(self$time_points, seq(0,1,length.out=self$num_buckets_time + 1))
        #}else{
          #quantiles_time = c(-Inf, Inf)
        #}
          
        #FIXME
        #only one bucket for 
        #if(self$num_buckets_time == 1L){ 
          
        #FIXME ist lapply sehr viel besser? wie in 2d? 
        for (b in seq_len(self$num_buckets)){
          for(c in seq_len(self$num_buckets_time)){
            buckets = c(buckets, 
                        ProbRange2D$new(lower = quantiles[b], 
                                                 upper = quantiles[b+1], 
                                                 lower_time = quantiles_time[c] ,
                                                 upper_time = quantiles_time[c+1]))
            
            
          }
        }
        #}
        

        buckets[[2]]$lower = -Inf
        buckets[[2]]$lower_time = -Inf
        buckets[[length(buckets)]]$upper = Inf
        buckets[[length(buckets)]]$upper_time = Inf
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
    

    get_masked  = function(data,resid, idx, probs, bucket){
      mask = bucket$in_range_mask(probs[idx,])
      #FIXME geht das?
      #if (sum(mask) < 1L) next # case no obs. are in the bucket. Are assigned corrs=0.
      data_m = data[idx,][mask$mask,]
      resid_m = resid[idx,][mask$mask, mask$mask_time]
      idx_m = idx[mask$mask]
      
      return (list(data_m = data_m, resid_m = resid_m, idx_m = idx_m))
    },
    
    get_probs = function(pred_probs, new_probs){
      #FIXME do not always calculate again? Does it make sense to take mean?  
      if (self$rebucket) {
        probs = new_probs
      } else {
        probs = pred_probs
      }
    }
    
  )
)
