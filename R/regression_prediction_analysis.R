#' Prediction Analysis for Regression
#'
#' @import dplyr
#' @import scales
#' @import ggplot2
#'
#' @param known a vector of known values
#' @param predicted a matrix of predicted values. Matrix should be organized in
#' way that the first column represents the point estimate and the second and
#' third columns represent the lower and upper bound of the predictive interval
#' @param digits number of digits controling floats precision
#' @param base_size ggplot parameter to control text size in plots
#'
#' @return a list with the follwing compoments:
#' \describe{
#' \item{metrics}{a data.frame with the most relevant performance metrics for regression}
#' \item{plots}{a list storing three regression analysis plots}
#' }

regression_prediction_analysis <- function(
  known,
  predicted,
  digits = 2,
  base_size = 14
) {


  # Define Plot Domain ----
  y_range <- as.vector(extrapolation_interval(known))
  y_min <- y_range[1]
  y_max <- y_range[2]
  ylim <- c(min = y_min, max = y_max)

  # Predicted Data ----
  predicted_vector <- predicted[, 1]
  predicted_range <- predicted[, -1]

  # Compute Metrics ----
  mae <- mean_absolute_error(known = known, predicted = predicted_vector)
  rmse <- root_mean_squared_error(known = known, predicted = predicted_vector)
  nrmse <- root_mean_squared_error(known = known, predicted = predicted_vector, nrmse = T)
  rsquared <- r_squared(known = known, predicted = predicted_vector)
  bias <- prediction_bias(known = known, predicted = predicted_vector)
  coverage <- prediction_coverage(known = known, predicted = predicted_range)
  piw <- prediction_interval_width(predicted = predicted_range)

  metrics_vector <- c(
    MAE = mae, RMSE = rmse, NRMSE = nrmse,
    `R Squared` = rsquared, Bias = bias,
    Coverage = coverage, piw
  )

  # Metrics Frame ----
  metrics_frame <- round(bind_rows(metrics_vector), digits = digits)

  # Prediction Scatter Plot ----
  title <- "Prediction Analysis"
  subtitle <- paste0(
    "MAE: ", metrics_frame[1],
    "; RSME: ", metrics_frame[2],
    "; R Squared: ", metrics_frame[3]
  )
  identity_data <- data.frame(Known = y_min:y_max, Predicted = y_min:y_max)
  scatter_data <-data.frame(Known = known, Predicted = predicted_vector)
  prediction_scatter_plot <- ggplot(data = identity_data) +
    geom_line(mapping = aes(x = Known, y = Predicted),
      linetype = "dashed", colour = "red", show.legend = F) +
    geom_point(data = scatter_data, mapping = aes(x = Known, y = Predicted)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_classic(base_size = base_size) +
    ggtitle(label = title, subtitle = subtitle)

  # Prediction Residual Plot ----
  title <- "Residual Analysis"
  subtitle <- paste0("Prediction Bias: ", round(bias, digits = 2))
  residual_data <- data.frame(Known = known, Residual = known - predicted_vector)
  residual_plot <- ggplot(data = residual_data) +
    geom_point(mapping = aes(x = Known, y = Residual)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "red", size = 1,
      show.legend = F) +
    stat_smooth(mapping = aes(x = Known, y = Residual), method = "lm", se = F) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
    theme_classic(base_size = base_size) +
    ggtitle(label = title, subtitle = subtitle)

  # Predictive Interval Analysis ----
  # Sort Data According to Predicted
  index <- seq_len(nrow(predicted))
  predicted_order <- order(predicted_vector)
  known <- known[predicted_order]
  predicted <- predicted[predicted_order, ]

  piw_data <- data.frame(
    Index = index,
    Known = known,
    Center = predicted[, 1],
    Lower = predicted[, 2],
    Upper = predicted[, 3]
  )

  title <- "Predictive Interval Analysis"
  subtitle <- paste0(
    "Average Predictive Interval Width: ", metrics_frame[6],
    "\nCoverage Probability: ", metrics_frame[5]
  )
  predictive_interval_plot <- ggplot(data = piw_data) +
    geom_point(mapping = aes(x = Index, y = Known)) +
    geom_errorbar(
      mapping = aes(x = Index, ymin = Lower, ymax = Upper, alpha = 0.25),
      show.legend = FALSE
    ) +
    geom_line(
      mapping = aes(x = Index, y = Center),
      linetype = "dashed", colour = "red", show.legend = F, size = 1
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_classic(base_size = base_size) +
    ggtitle(label = title, subtitle = subtitle)

  # Return  Object ----
  object <- list(
    metrics = metrics_frame,
    plot = list(
      scatter = prediction_scatter_plot,
      residual = residual_plot,
      piw = predictive_interval_plot
    )
  )

  # return ----
  rout <- object
  return(rout)

}
