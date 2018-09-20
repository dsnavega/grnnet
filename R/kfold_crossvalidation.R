#' K-Fold Cross-Validation
#'
#' Helper function to perform K-Fold Cross-Validation
#'
#' @param n number of training examples
#' @param k number of folds. Default is 5
#' @param seed seed for the pseudo-random number generator
#'
#' @return a matrix of logical vectors defining observations in each fold.
#'

kfold_crossvalidation <- function(n, k = 5, seed = NULL) {

  # random number generator ----
  set.seed(seed = seed)

  # create folds and k-fold matrix ----
  x <- seq_len(n)
  kfold <- sample(cut(x = x, breaks = k, labels = FALSE))

  kfold_matrix <- matrix(NA, nrow = n, ncol = k)
  for(i in seq_len(k)) {
    kfold_matrix[, i] = kfold == i
  }

  colnames(kfold_matrix) <- seq_len(k)
  rownames(kfold_matrix) <- seq_len(n)

  # return ----
  rout <- kfold_matrix
  return(rout)

}
