clip_prob = function(prob) {
  prob[prob > 1] = 1
  prob[prob < 0] = 0
  return(prob)
}

#' One-hot encode a factor variable
#' @export
one_hot = function(labels) {
  con = contrasts(labels, contrasts = FALSE)
  mat = con[as.integer(labels),]
  rownames(mat) = NULL
  return(mat)
}

xy_to_task = function(x, y) {
  x = data.table::data.table(x)
  yname = "ytmp"

  # Safe yname
  i = 0
  while(yname %in% names(x)) {
    i = i + 1
    yname = "ytmp"
    yname = paste0(yname, i)
  }

  x[, (yname) := y]
  if (is.numeric(y)) {
    ti = TaskRegr
  } else {
    ti = TaskClassif
  }
  ti$new(id = "tmptsk", backend = x, target = yname)
}