#' Compute Descriptor
#'
#' \emph{compute_descriptor} is wrapper for the \code{\link[base]{apply}}
#' function to facilitate row or column wise operations on matrix and data.frame
#' objects.
#'
#' @param x A matrix or data.frame
#' @param fun A function defining a descriptor to apply to x.
#'
#' fun must must operate on a vector basis.
#' The rows or columns of x will be passed as the first argument.
#' Other arguments can be passed with \strong{...}
#'
#' @param by A character vector. "row" defines row-wise and "col" defines
#' column-wise operation.
#' @param ... Additional arguments to pass to \emph{fun}
#'
#' @return A vector, data.frame/matrix or a list with the computed descriptor
#' values
#'
#' @examples
#' df <- iris[,-5]
#' # Compute the mean of iris sepal and petal dimensions. Output is a vector.
#' compute_descriptor(df, fun = mean, by = "col")
#' # Compute the range of iris sepal and petal dimensions. Output is a list
#' compute_descriptor(df, fun = range, by = "col")
#'
compute_descriptor <- function(x, fun, by = "col", ...) {

    condition <- is.matrix(x) | is.data.frame(x)
    if(condition) {

        descriptor <- switch(
            by,
            row = { apply(x, MARGIN = 1, FUN = fun, ...) },
            col = { apply(x, MARGIN = 2, FUN = fun, ...) },
            { stop("by must be 'col' or 'row'.")}
        )

        # return
        rout <- descriptor
        return(rout)

    } else {

        stop("x must be a matrix or a data.frame object.")

    }

}
