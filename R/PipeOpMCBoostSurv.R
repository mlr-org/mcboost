PipeOpMCBoostSurv = R6Class("PipeOpMCBoostSurv",
                        inherit = mlr3pipelines::PipeOp,
                        public = list(
                          
         
                          initialize = function(id = "mcboostsurv", param_vals = list()) {
                            param_set = paradox::ParamSet$new(list(
                              paradox::ParamInt$new("max_iter", lower = 0L, upper = Inf, default =5L, tags = "train"),
                              paradox::ParamDbl$new("alpha", lower = 0, upper = 1, default = 1e-4, tags = "train"),
                              paradox::ParamDbl$new("eta", lower = 0, upper = 1, default = 1, tags = "train"),
                              #paradox::ParamLgl$new("partition", tags = "train", default = TRUE),
                              paradox::ParamInt$new("num_buckets", lower = 1, upper = Inf, default = 2L, tags = "train"),
                              paradox::ParamInt$new("time_buckets", lower = 1, upper = Inf, default = 1L, tags = "train"),
                              paradox::ParamDbl$new("time_eval", lower = 0, upper = 1, default = 1, tags = "train"),
                              #bucket_strategy
                              #bucket_aggragation
                              #eval_fulldata
                              paradox::ParamLgl$new("rebucket", default = FALSE, tags = "train"),
                              paradox::ParamLgl$new("multiplicative", default = TRUE, tags = "train"),
                              paradox::ParamUty$new("auditor_fitter", default = NULL, tags = "train"),
                              paradox::ParamUty$new("subpops", default = NULL, tags = "train"),
                              #paradox::ParamUty$new("default_model_class", default = ConstantPredictor, tags = "train"),
                              paradox::ParamUty$new("init_predictor", default = NULL, tags = "train")
                            ))
                            super$initialize(id, param_set = param_set, param_vals = param_vals, packages = character(0),
                                             input = data.table(name = c("data", "prediction"), train = c("TaskSurv", "TaskSurv"), predict = c("TaskSurv", "TaskSurv")),
                                             output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
                                             tags = "target transform")
                          }
                        ),
                        private = list(
                          .train = function(inputs) {
                            d = inputs$data$data(cols = inputs$data$feature_names)
                            l = inputs$data$data(cols = inputs$data$target_names)
                            
                            args = self$param_set$get_values(tags = "train")
                            
                            if (is.null(args$init_predictor)) {
                              # Construct an initial predictor from the input model if non is provided.
                              init_predictor = function(data, prediction) {
                                # Prob or response prediction
                                if (length(prediction$feature_names) > 1L) {
                                  prds = prediction$data(cols = prediction$feature_names)
                                  as.matrix(prds)
                                } else {
                                  prds = prediction$data(cols = prediction$feature_names)[[1]]
                                  one_hot(prds)
                                }
                              }
                              args$init_predictor = init_predictor
                            }
                            mc = invoke(MCBoostSurv$new, .args = args)
                            mc$multicalibrate(d, l, predictor_args = inputs$prediction)
                            self$state = list("mc" = mc)
                            list(NULL)
                          },
                          
                          .predict = function(inputs) {
                            
                            #FIXME geht das so? 
                            d = inputs$data$data(cols = inputs$data$feature_names)
                            probs = self$state$mc$predict_probs(d, predictor_args = inputs$prediction)
         
                            distlist = lapply(seq_len(nrow(probs)), function(x){
                              distr6::WeightedDiscrete$new(x=as.numeric(colnames(probs)), cdf = 1-probs[x,])
                            }
        
                            vector_distribution = distr6::VectorDistribution$new(distlist = distlist)
                            
                            list(PredictionSurv$new(
                              inputs$prediction,
                              row_ids = inputs$prediction$row_ids,
                              truth = inputs$prediction$truth(),
                              distr = vector_distribution
                            ))
                          }
                        ),
                        active = list(
                          #' @field predict_type Predict type of the PipeOp.
                          predict_type = function(val) {
                            if (!missing(val)) {
                              if (!identical(val, private$.learner)) {
                                stop("$predict_type for PipeOpMCBoostSurv is read-only.")
                              }
                            }
                            return("distr")
                          }
                        )
)


ppl_mcboostsurv = function(learner = lrn("surv.kaplan")) { #FIXME Can this be replaced with featureless? 
  mlr3misc::require_namespaces("mlr3pipelines")
  po_lrn = mlr3pipelines::po("learner_cv", learner = learner, resampling.method = "insample")
  gr = mlr3pipelines::`%>>%`(
    mlr3pipelines::gunion(list(
      "data" = mlr3pipelines::po("nop"),
      "prediction" = po_lrn
    )),
    PipeOpMCBoostSurv$new()
  )
}