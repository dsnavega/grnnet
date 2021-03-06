#' Pairwise Squared Euclidean Distance
#'
#' Fast Pairwise Squared Euclidean Distance Computation Using Formula Expansion.
#'
#' @author David Navega
#'
#' @param x a numeric matrix
#' @param y a numeric matrix with the same number of columns as matrix x
#' @param weights a numeric vector matching the number of columns defining the
#' weights for a weighted distance vector.
#'
#' @return a matrix with euclidean distances between rows of x and y
#'
pw_euclidean_distance <- function(x, y, weights = NULL) {

    # x & y dimensions
    x_dimensions <- dim(x)
    y_dimensions <- dim(y)

    n <- x_dimensions[1]
    m <- y_dimensions[1]

    condition <- is.matrix(x) & is.matrix(y)
    if(condition) {

        condition <- x_dimensions[2] == y_dimensions[2]
        if(condition) {

            condition <- is.null(weights)
            if(condition) {

                weights <- rep(1, x_dimensions[2])
                weights <- diag(weights)

            } else {

                condition <- length(weights) != x_dimensions[2]
                if(condition) {
                    stop("[-]Number of weights don't match number of features.")
                } else {

                    condition <- is.vector(weights)
                    if(condition) {
                        weights <- diag(weights)
                    }

                }

            }

            # X Dot Product
            # x_dot <- diag(tcrossprod(x, x))
            x_dot <- diag(x %*% weights %*% t(x))
            x_dot <- matrix(x_dot, nrow = n, ncol = m)
            x_dot <- round(x_dot, digits = 1)

            # Y Dot Product
            # y_dot <- diag(tcrossprod(y, y))
            y_dot <- diag(y %*% weights %*% t(y))
            y_dot <- matrix(y_dot, nrow = n, ncol = m, byrow = T)
            y_dot <- round(y_dot, digits = 1)

            # XY Dot Product
            # xy_dot <- t(tcrossprod(y, x))
            xy_dot <- t(y %*% weights %*% t(x))
            xy_dot <- round(xy_dot, digits = 1)

            # Euclidean Distance Formula Expansion
            formula_expansion <- x_dot + y_dot - 2 * xy_dot
            formula_expansion[formula_expansion < 0] <- 0
            euclidean_distance <- sqrt(formula_expansion)

            # return
            rout <- euclidean_distance
            return(rout)

        } else {
            stop("[-]Number of columns in x and y must be the same.")
        }

    } else {
        stop("[-]x and y must be matrices.")
    }

}
