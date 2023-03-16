#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------

calculate.accuracy <- function(predictions, ref.labels) {
  return(length(which(predictions == ref.labels)) / length(ref.labels))
}

calculate.w.accuracy <- function(predictions, ref.labels, weights) {
  lvls <- levels(ref.labels)
  if (length(weights) != length(lvls)) {
    stop("Number of weights should agree with the number of classes.")
  }
  if (sum(weights) != 1) {
    stop("Weights do not sum to 1")
  }
  accs <- lapply(lvls, function(x) {
    idx <- which(ref.labels == x)
    return(calculate.accuracy(predictions[idx], ref.labels[idx]))
  })
  acc <- mean(unlist(accs))
  return(acc)
}

calculate_multiclass_accuracy <- function(predictions, ref.labels){
  overall_acc <- calculate.accuracy(predictions, ref.labels)
  weights <- rep(1 / length(levels(ref.labels)), length(levels(ref.labels)))
  w.acc <- calculate.w.accuracy(predictions, ref.labels, weights)
  return(list(overall_acc=overall_acc, weighted_acc=w.acc))
}