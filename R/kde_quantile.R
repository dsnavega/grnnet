#' Quantile Estimation using Kernel Density Estimation
#'
#'
#' @param x a numeric vector
#' @param probs quantiles (cumulative probability value) to assess
#'
#' @return a named vector of estimated quantiles
#'
kde_quantile <- function(x, probs = c(0.001, 0.999)) {

    # evaluate density
    kde <- density(x, na.rm = TRUE)
    kde_range <- range(kde$x)

    # approximate cumulative density function from kde object
    cdf <- approxfun(x = kde$x, y = cumsum(kde$y / sum(kde$y)))

    # compute quantile with vectorization function
    compute_quantile <- function(q) {

        root_wrapper <- function(x, q) {

            value <- cdf(x) - q

            # return
            rout <- value
            return(rout)
        }

        root_value <- uniroot(root_wrapper, interval = kde_range, q = q)$root

        # return
        rout <- root_value
        return(rout)

    }
    compute_quantile <- Vectorize(compute_quantile)

    # quantile estimation
    kde_quantiles <- compute_quantile(probs)
    qnames <- paste0(round(probs, digits = 4) * 100, "%")
    names(kde_quantiles) <- qnames

    # return
    rout <- kde_quantiles
    return(rout)

}
