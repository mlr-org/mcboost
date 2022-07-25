#' One-hot encode a factor variable
#' @param labels [`factor`]\cr
#'   Factor to encode.
#' @examples
#'  \dontrun{
#'  one_hot(factor(c("a", "b", "a")))
#'  }
#' @return [`integer`]\cr
#'   Integer vector of encoded labels.
#' @export
one_hot = function(labels) {
  con = contrasts(labels, contrasts = FALSE)
  mat = con[as.integer(labels), ]
  rownames(mat) = NULL
  if (ncol(mat) == 2L) mat = mat[, 1L]
  return(mat)
}


# clip numbers (probabilities) to [0;1]
clip_prob = function(prob) {
  prob[prob > 1] = 1
  prob[prob < 0] = 0
  return(prob)
}


# Convert a X,y pair to a task
# Required for interacting with 'mlr3'
xy_to_task = function(x, y) {

  x = data.table::data.table(x)
  yname = "ytmp"

  # Safe yname
  i = 0
  while (yname %in% names(x)) {
    i = i + 1
    yname = paste0("ytmp", i)
  }

  x[, (yname) := y]

  if (is.numeric(y)) {
    ti = mlr3::TaskRegr
  } else {
    ti = mlr3::TaskClassif
  }
  ti$new(id = "tmptsk", backend = x, target = yname)
}

#' Create an initial predictor function from a trained mlr3 learner
#'
#' @param learner [`mlr3::Learner`]
#'   A trained learner used for initialization.
#' @examples
#'  \dontrun{
#'  library("mlr3")
#'  l = lrn("classif.featureless")$train(tsk("sonar"))
#'  mlr3_init_predictor(l)
#'  }
#' @return [`function`]
#' @export
mlr3_init_predictor = function(learner) {
  if (is.null(learner$state)) stop("Learner needs to be trained first!")
  if (learner$predict_type == "response") {
    function(data, ...) {
      one_hot(learner$predict_newdata(data)$response)
    }
  } else if ("distr" %in% learner$predict_types) {
    function(data, ...) {
      as.data.table(learner$predict_newdata(data))$distr[[1]][[1]]
    }
  } else if(learner$predict_type == "prob") {
    function(data, ...) {
      learner$predict_newdata(data)$prob[, 1L]
    }
  } else{
    stop("Predict type of your learner is not implemented. (response, distr, prob)")
  }
}



#' Create even intervals
#' @param frac [`numeric`]
#' number of buckets
#' @param min [`numeric`]
#' maximum value
#' @param max [`numeric`]
#' minimum value
#' @return [`numeric`]
#' @noRd
even_bucket = function(frac, min, max) {
  pos = c(0, seq_len(frac))
  min + pos / frac * (max - min)
}
