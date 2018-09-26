#' Evaluate \emph{grnnet} model
#'
#' @export
#' @author David Navega
#'
#' @param object a \emph{grnnet} object
#' @param digits number of digits in computed metrics
#'
#' @return relevant metrics for \emph{grnnet} output
#'
#' @examples
#' model <- grnnet(iris[,1:2], iris[,3:4])
#' evaluate_grnnet(model)
#'
evaluate_grnnet <- function(object, digits = 2) {

  condition <- class(object) != "grnnet"
  if(condition) {
    stop("[-] object class must be 'grnnet'")
  }

  condition <- object$crossvalidation != T
  if(condition) {
    stop("[-] 'grnnet' model must have been crossvalidated during training.")
  }

  known <- object$data$y
  predicted <- object$predicted
  m <- ncol(known)

  condition <- m > 1
  if(condition) {

    metrics_list <- lapply(seq_len(m), function(i) {

      analysis <- regression_prediction_analysis(
        known = known[, i],
        predicted = predicted[[i]]
      )

      analysis$metrics

    })

    names(metrics_list) <- colnames(known)

    evaluated <- bind_rows(metrics_list)

  } else {

    analysis <- regression_prediction_analysis(
      known = known,
      predicted = predicted
    )

    evaluated <- analysis$metrics

  }

  # return
  rout <- evaluated
  return(rout)

}
