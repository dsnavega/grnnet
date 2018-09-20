#' Normalize Data
#'
#' Normalize data using min-max scaling. Normalized data is bounded between
#' 0 and 1
#'
#' @export
#'
#' @param x a matrix or data.frame to normalize
#' @param y an optional matrix or data.frame previously normalized from which
#' normalization paramter can be extracted.
#'
#' @return matrix or data.frame with normalized data
#'
normalize <- function(x, y = NULL){

    normalizer <- function(x, minimum = NULL, maximum = NULL) {

        condition <- is.null(minimum)
        if(condition){
            minimum <- min(x, na.rm = TRUE)
        }

        condition <- is.null(maximum)
        if(condition){
            maximum <- max(x, na.rm = TRUE)
        }

        # min-max scaling formaula
        normalized <- (x - minimum) / (maximum - minimum)

        # return
        rout <- normalized
        return(rout)
    }

    normalized <- apply(x, 2, normalizer)
    minimum <- apply(x, 2, min, na.rm = TRUE)
    maximum <- apply(x, 2, max, na.rm = TRUE)
    attr(normalized, 'minimum')  <- minimum
    attr(normalized, 'maximum')  <- maximum

    condition <- !is.null(y)
    if(condition){

        minimum <- attr(y, 'minimum')
        maximum <- attr(y, 'maximum')

        condition <- NCOL(x) != NCOL(y)
        if(condition){
            stop('Number of predictors in x do not match predictors in y.')
        }

        normalized <- do.call(cbind, lapply(seq_len(NCOL(x)), function(i){
            normalizer(
                x = x[ , i],
                minimum = min(c(min(x[, i], na.rm = T), minimum[i])),
                maximum = max(c(max(x[, i], na.rm = T), maximum[i]))
            )
        }))

    }

    # return ----
    rout <- normalized
    return(rout)

}
