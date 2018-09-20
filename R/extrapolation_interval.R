#' Extrapolation Interval
#'
#' @param x a numeric vector
#' @return the extrapolation interval of x
#'
extrapolation_interval <- function(x) {

    x_quantile <- quantile(x, probs = c(0.001, 0.999), na.rm = T)
    k_quantile <- kde_quantile(x, probs = c(0.001, 0.999))

    minimum <- max(c(x_quantile[1], k_quantile[1]))
    maximum <- max(c(x_quantile[2], k_quantile[2]))

    interval <- unname(c(minimum, maximum))

    # return
    rout <- interval
    return(rout)

}
