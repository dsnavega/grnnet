#' Weighted Standard Deviation
#'
#' @param x a numeric vector
#' @param weights a numeric vector of weights
#' @param na.rm a logical for handle NA values
#' @param w_mean pre-computed mean
#'
#' @return weighted standard deviation
#'
weighted_sd <- function(x, weights, na.rm = F, w_mean) {

  if(missing(w_mean)) {
    w_mean <- sum(x * weights, na.rm = na.rm) / sum(weights, na.rm = na.rm)
  }

  w_var <- sum(((x - w_mean) ^ 2) * weights, na.rm = na.rm) /
    sum(weights,na.rm = na.rm)

  value <- sqrt(w_var)

  # return
  rout <- value
  return(rout)

}
