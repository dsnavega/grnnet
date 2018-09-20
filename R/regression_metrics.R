#' Mean Absolute Error (MAE)
#'
#' Evaluates MAE on predictions of a regression model
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (point estimates)
#'
#' @return MAE value
#'
mean_absolute_error <- function(known, predicted) {

  mae <- mean(abs(known - predicted), na.rm = T)
  # return
  rout <- mae
  return(rout)

}


#' Root Mean Squared Error (RMSE)
#'
#' Evaluates RMSE on predictions of a regression model
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (point estimates)
#' @param nrmse a logical indicating if normalized rmse should be computed.
#' Default is FALSE (F)
#'
#' @return RMSE (or NRMSE) value
#'
root_mean_squared_error <- function(known, predicted, nrmse = F) {

  rmse <- sqrt(mean((known - predicted) ^ 2, na.rm = T))
  condition <- nrmse
  if(condition) {
    rmse <- sqrt(mean((known - predicted) ^ 2, na.rm = T)) / IQR(known)
  }
  # return
  rout <- rmse
  return(rout)

}


#' R Squared
#'
#' Evaluates R Squared, Explained Variance, on predictions of a regression model
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (point estimates)
#'
#' @return R Squared value
#'
#' @note A negative R Squared value means that the prediction model is worst
#' than fitting an horizontal line to the data.
#'
r_squared <- function(known, predicted) {

  # total sum of squares
  tss <- sum((known - mean(known, na.rm = T)) ^ 2, na.rm = T)
  # explained sum of squares
  ess <- sum((predicted - mean(known)) ^ 2, na.rm = T)
  # residual sum of squares (sum of squared errors)
  rss <- sum((known - predicted) ^ 2, na.rm = T)
  # r squared
  rsquared <-  1 - (rss / tss)

  # return
  rout <- rsquared
  return(rout)

}


#' Regression Prediction Bias
#'
#' Evaluate Regression Prediction Bias by the slope of the regression model of
#' residuals on known values.
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (point estimates)
#'
#' @return Regression prediction bias value
#'
#' @note A positive bias means that lower known values are overestimated and
#' upper known values are underestimated.
#'
prediction_bias <- function(known, predicted) {

  residual <- known - predicted

  Sxy <- sum((known - mean(known)) * (residual - mean(residual)))
  Sxx <- sum((known - mean(known)) ^ 2)
  slope <- Sxy / Sxx

  # return
  rout <- slope
  return(rout)

}

#' Prediction Coverage
#'
#' Evaluates the coverage of prediction intervals.
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (prediction interval, lower and upper value)
#'
#' @return Coverage probability value
#'
prediction_coverage <- function(known, predicted) {

  n <- length(known) - sum(is.na(predicted[, 1]))
  predicted <- t(apply(predicted, MARGIN = 1, FUN = range))

  hits <- sapply(seq_len(n), function(i) {
    known[i] >= predicted[i, 1] & known[i] <= predicted[i, 2]
  })

  coverage <- sum(hits, na.rm = T) / n

  # return
  rout <- coverage
  return(rout)
}

#' Prediction Interval Mean Width
#'
#' Evaluates mean width of prediction intervals as a measure of efficiency
#'
#' @export
#'
#' @param predicted predicted values (prediction interval, lower and upper value)
#'
#' @return a vector width mean width and percentiles of prediciton interval width
#' (0.025, 0.975)
#'
prediction_interval_width <- function(predicted) {

  range_value <- function(x) {

    condition <- any(is.infinite(x) | is.na(x))
    if(condition) {
      rout <- NA
    } else {
      internal_range <- range(x, na.rm = T)
      rout <- internal_range[2] - internal_range[1]
    }

    # return
    return(rout)

  }

  p_vector <-c(0.5, 0.025, 0.975)
  piw_names <- c("PIW", "PIW (0.025)", "PIW (0.975)")
  range_values <- apply(predicted, MARGIN = 1, FUN = range_value)
  piw <- quantile(range_values, probs = p_vector, na.rm = T)
  piw[1] <- mean(range_values, na.rm = T)
  piw <- as.numeric(piw)
  names(piw) <- piw_names

  # return
  rout <- piw
  return(rout)

}

#' Adjusted R Squared
#'
#' Evaluates Adjusted R Squared, Explained Variance, on predictions of a
#' regression model.
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (point estimates)
#' @param p number of predictors used in regression model
#'
#' @return Adjusted R Squared value
#'
#' @note A negative R Squared value means that the prediction model is worst
#' than fitting an horizontal line to the data.
#'
adjusted_r_squared <- function (known, predicted, p) {

  # cases
  n <- length(known)

  # r squared
  r2 <- r_squared(known, predicted)

  # adjusted.rsquared
  adj_r2 <- r2 - (1 - r2) * (p / (n - p - 1))

  # return
  rout <- adj_r2
  return(rout)

}

#' Weighted R Squared
#'
#' Evaluates Weighted R Squared, Explained Variance, on predictions of a
#' regression model.
#'
#' @export
#'
#' @param known known values
#' @param predicted predicted values (point estimates)
#' @param weights a vector of weights
#'
#' @return Weighted R Squared value
#'
#' @note A negative R Squared value means that the prediction model is worst
#' than fitting an horizontal line to the data.
#'
weighted_r_squared <- function(known, predicted, weights) {
  w <- weights
  # total sum of squares
  tss <- sum(w * (known - mean(known)) ^ 2, na.rm = T) / sum(w, na.rm = T)
  # explained sum of squares
  ess <- sum(w * (predicted - mean(known)) ^ 2, na.rm = T) / sum(w, na.rm = T)
  # residual sum of squares (sum of squared errors)
  rss <- sum(w * (known - predicted) ^ 2, na.rm = T) / sum(w, na.rm = T)
  # r squared
  rsquared <-  1 - (rss / tss)

  # return
  rout <- rsquared
  return(rout)

}
