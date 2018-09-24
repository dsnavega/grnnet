#' Squared Exponential Kernel
#'
#' @author David Navega
#'
#' @param x a vector or a matrix of pre-computed euclidean distances
#' @param sigma a numeric defining the spread of the gaussian kernel
#'
#' @return evaluation values of the squared exponential kernel function
#'
squared_exponential_kernel <- function(x, sigma) {

    value <- exp(- (x ^ 2) / (2 * sigma ^ 2))

    # return
    rout <- value
    return(rout)

}
