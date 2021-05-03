#' One-hot encode a factor variable
#' @param labels [`factor`]\cr
#'   Factor to encode.
#' @export
one_hot = function(labels) {
  con = contrasts(labels, contrasts = FALSE)
  mat = con[as.integer(labels),]
  rownames(mat) = NULL
  if (ncol(mat) == 2L) mat = mat[,1L]
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
  while(yname %in% names(x)) {
    i = i + 1
    yname = paste0("ytmp", i)
  }

  x[, (yname) := y]
  if (is.numeric(y)) {
    ti = TaskRegr
  } else {
    ti = TaskClassif
  }
  ti$new(id = "tmptsk", backend = x, target = yname)
}
