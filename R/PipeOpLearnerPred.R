PipeOpLearnerPred = R6Class("PipeOpLearnerPred",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      private$.learner$param_set$set_id = ""
      id = id %??% private$.learner$id
      task_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$task
      super$initialize(id, alist(private$.learner$param_set), 
                       param_vals = param_vals, 
                       can_subset_cols = TRUE, 
                       task_type = task_type, 
                       tags = c("learner"))
    }
    
  ),
  active = list(
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      private$.learner
    },
    learner_model = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      if (is.null(self$state) || is_noop(self$state)) {
        private$.learner
      } else {
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learner)
      }
    }
  ),
  private = list(
    .train_task = function(task) {
      on.exit({private$.learner$state = NULL})
      
      # Train a learner for predicting
      self$state = private$.learner$train(task)$state
      prds = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prds, task)
    },
    
    .predict_task = function(task) {
      on.exit({private$.learner$state = NULL})
      private$.learner$state = self$state
      prediction = as.data.table(private$.learner$predict(task))
      private$pred_to_task(prediction, task)
    },
    
    pred_to_task = function(prds, task) {
      renaming = setdiff(colnames(prds), c( "row_ids"))
      setnames(prds, renaming, sprintf("%s.%s", self$id, renaming))
      setnames(prds, old = "row_ids", new = task$backend$primary_key)
      task$select(character(0))$cbind(prds)
    },
    .learner = NULL
  )
)
mlr_pipeops$add("learner_pred", PipeOpLearnerPred, list(R6Class("PipeOpLearnerPred", public = list(id = "learner_pred", task_type = "surv", param_set = ParamSet$new()))$new()))

