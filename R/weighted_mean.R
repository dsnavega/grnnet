#' Weighted Mean
#'
#' @param x a numeric vector
#' @param weights a numeric vector of weights
#' @param na.rm a logical for handle NA values
#'
#' @return weighted mean
#'
weighted_mean <- function(x, weights, na.rm = FALSE) {

    value <- sum(x * weights, na.rm = na.rm) / sum(weights, na.rm = na.rm)

    # return
    rout <- value
    return(rout)

}
