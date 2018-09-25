#' Generalized Regression Neural Network
#'
#' Implementation of the Generalized Regression Neural Network architecture for
#' Single and Multi-Output Regression with Uncertainty Estimation Using
#' Conformal Prediction.
#'
#' @author David Navega
#'
#' @export
#'
#' @param x a matrix of numeric inputs
#' @param y a matrix of numeric outputs
#' @param alpha a numeric value. confidence level (i.e, 0.05)
#' @param crossvalidation a logical indicating if k-fold cross-validation should
#' be performed. Default is TRUE (T)
#' @param kfold number of fold for k-fold cross-validation
#' @param seed seed for the random number generator. Controls reproducibility of
#' k-fold cross-validation
#'
#' @return a trained generalized regression neural network
#'
#' @note
#' The network training is performed with an internal leave-one-out sampling
#' loop and derivate free optimization (Brent's algorithm). The bounds of the
#' sigma parameter, the spread of the squared exponential kernel, are initialized
#' following Tomandl & Schober (2001) based on the proprieties of the distance
#' matrix between cases. The internal loop also computes the conformity factors
#' needed to compute the prediction intervals based on conformal prediction.
#'
grnnet <- function(x, y,
  alpha = 0.05,
  crossvalidation = T,
  kfold = 5,
  seed = 1984
) {

  condition <- alpha <= 0.0 | alpha >= 1.0
  if(condition) {
    stop("[-]alpha must be between 0.00 and 1.00")
  }

  condition <- any(is.na(x)) | any(is.na(y))
  if(condition) {
    stop("[-]NA values detected. Remove NA values in x or y.")
  } else {

    condition <- NROW(x) != NROW(y)
    if(condition) {
      stop("[-]Number of observations (rows) in x and y do not match.")
    } else {

      # coercion ----
      x <- as.matrix(x)
      y <- as.matrix(y)

      # number of observations, features and output
      ncases <- nrow(x)
      nfeatures <- ncol(x)
      noutput <- ncol(y)

      # extrapolation intervals for outputs
      interval <- compute_descriptor(
        x = y[,,drop = F],
        fun = extrapolation_interval,
        by = "col"
      )

      # initialize distance matrix ----
      distance_matrix <- pw_euclidean_distance(
        x = x, y = x
      )

      # bounding values for smoothing parameter ----
      # # Eq. (30) & (36) Tomandl & Schober [2001]
      # phi <- (2 * log(.Machine$double.eps))
      # lower <- sqrt(min(distance_matrix[distance_matrix != 0]) / phi)
      # upper <- 0.5 * max(distance_matrix)

      optimization_bounds <- range(distance_matrix[distance_matrix != 0])
      lower <- sqrt(0.5 * optimization_bounds[1])
      upper <- sqrt(0.5 * optimization_bounds[2])

      # wrapper for optimize() ----
      # generalized regression neural network training wrapper ---
      optimization_wrapper <- function(sigma) {

        predicted <- sapply(seq_len(ncases), function(i) {

          weights <- squared_exponential_kernel(
            x = distance_matrix[-i, i],
            sigma = sigma
          )

          compute_descriptor(
            x = y[-i,, drop = F],
            fun = weighted_mean,
            by = "col",
            weights = weights
          )

        })

        condition <- noutput > 1
        if(condition) {
          predicted <- t(as.matrix(predicted))
        } else {
          predicted <- as.matrix(predicted)
        }

        nrmse_vector <- sapply(seq_len(noutput), function(j) {

          root_mean_squared_error(
            known = y[, j],
            predicted = predicted[, j],
            nrmse = T
          )

        })

        mean_nrmse <- mean(nrmse_vector)

        # return
        rout <- mean_nrmse
        return(rout)

      }

      # optimize sigma -> train network ----
      optimal_grnnet <- optimize(
        f = optimization_wrapper,
        interval = c(lower, upper)
      )

      optimal_sigma <- optimal_grnnet$minimum

      # weights matrix given optimal sigma ----
      weights_matrix <- squared_exponential_kernel(
        x = distance_matrix, sigma = optimal_sigma
      )

      # generate network output ----
      network_output <- lapply(seq_len(ncases), function(i) {

        output_function <- function(x, ...)  {

          output_value <- c(weighted_mean(x,...), weighted_sd(x,...))

          # return ----
          rout <- output_value
          return(rout)
        }

        compute_descriptor(
          x = y[-i,, drop = F],
          fun = output_function,
          by = "col",
          weights = weights_matrix[i, -i]
        )

      })

      # manipulate output ---
      output_manipulator <- function(x) {

        output_estimate <- sapply(seq_len(ncases), function(i) {
          x[[i]][1, ]
        })

        output_variance <- sapply(seq_len(ncases), function(i) {
          x[[i]][2, ]
        })

        condition <- noutput > 1
        if(condition) {

          output_list <- list(
            estimate = t(as.matrix(output_estimate)),
            variance = t(as.matrix(output_variance))
          )

        } else {

          output_list <- list(
            estimate = as.matrix(output_estimate),
            variance = as.matrix(output_variance)
          )

        }


        # return
        rout <- output_list
        return(rout)

      }

      # clean output ----
      network_output <- output_manipulator(x = network_output)

      # conformal prediction ----
      estimate <- network_output$estimate
      variance <- network_output$variance

      network_error <- abs(y - estimate)

      conformity_score <- network_error / variance
      conformity_factor <- compute_descriptor(
        x = conformity_score,
        fun = quantile,
        probs = 1 - alpha
      )

      # cross-validation ----
      condition <- crossvalidation
      if(condition) {

        kfcv <- kfold_crossvalidation(
          n = ncases, k = kfold, seed = seed
        )

        condition <- noutput > 1
        if(condition) {

          predicted <- lapply(seq_len(noutput), function(i) {
            matrix(NA, nrow = ncases, ncol = 3)
          })
          names(predicted) <- colnames(y)

        } else {

          predicted <- matrix(NA, nrow = ncases, ncol = 3)

        }

        for(i in seq_len(kfold)) {

          fold <- kfcv[, i]
          train <- !fold
          test <- fold

          grnnet_object <- grnnet(
            x = x[train,, drop = F],
            y = y[train,, drop = F],
            crossvalidation = F,
            alpha = alpha
          )

          grnnet_predicted <- predict(
            object = grnnet_object, newdata = x[test,, drop = F]
          )

          condition <- noutput > 1
          if(condition) {

            for(j in seq_len(noutput)) {
              predicted[[j]][test, ] <- grnnet_predicted[[j]]
            }

          } else {

            predicted[test, ] <- grnnet_predicted

          }

        }

      }

      condition <- crossvalidation
      network_structure <- list(
        data = list(x = x, y = y, interval = interval),
        sigma = optimal_sigma,
        alpha = alpha,
        crossvalidation = crossvalidation,
        predicted = if(condition) { predicted } else { NULL },
        estimate = estimate,
        variance = variance,
        conformity = list(
          factor = conformity_factor,
          score = conformity_score
        )
      )
      network_structure <- structure(network_structure, class = "grnnet")

      # return ----
      rout <- network_structure
      return(rout)


    }

  }

}

#' Predict from a Generalized Regression Neural Network Object
#'
#' @author David Navega
#' @export
#'
#' @param object object of class 'grnnet'
#' @param newdata a matrix or a data.frame of new data to make predictions
#' @param alpha a value between 0 and 1 defining the confidence level for
#' conformal prediction intervals. Default is NULL and alpha is equal to
#' pre-defined value during training.
#' @param ... ...
#'
#' @return a matrix or a list with a point estimate and associated prediction
#' interval for each row of newdata. A named list is outputed if the neural
#' network has multiple output
#'
predict.grnnet <- function(object, newdata, alpha = NULL, ...) {

  # coerce
  newdata <- as.matrix(newdata)

  # network parameterization
  x <- object$data$x
  y <- object$data$y
  interval <- object$data$interval
  sigma <- object$sigma


  ncases <- nrow(newdata)
  noutput <- ncol(y)

  condition <- is.null(alpha)
  if(condition) {
    alpha <- object$alpha
  } else {
    condition <- alpha <= 0.0 | alpha >= 1.0
    if(condition) {
      stop("[-]alpha must be between 0.00 and 1.00")
    }
  }

  condition <- any(is.na(newdata))
  if(condition) {
    stop("[-]NA values detected in newdata. Remove or impute NA values.")
  } else {

    condition <- ncol(newdata) != ncol(x)
    if(condition) {
      stop("[-]Number of features in newdata do not match trained grnnet.")
    } else {

      # distance matrix ----
      distance_matrix <- pw_euclidean_distance(x = x, y = newdata)

      # weights matrix given sigma ----
      weights_matrix <- squared_exponential_kernel(
        x = distance_matrix, sigma = sigma
      )

      # generate network output ----
      network_output <- lapply(seq_len(ncases), function(i) {

        output_function <- function(x, ...)  {

          output_value <- c(weighted_mean(x,...), weighted_sd(x,...))

          # return ----
          rout <- output_value
          return(rout)
        }

        compute_descriptor(
          x = y[,, drop = F],
          fun = output_function,
          by = "col",
          weights = weights_matrix[, i]
        )

      })

      # manipulate output ---
      output_manipulator <- function(x) {

        output_estimate <- sapply(seq_len(ncases), function(i) {
          x[[i]][1, ]
        })

        output_variance <- sapply(seq_len(ncases), function(i) {
          x[[i]][2, ]
        })

        condition <- noutput > 1
        if(condition) {

          output_list <- list(
            estimate = t(as.matrix(output_estimate)),
            variance = t(as.matrix(output_variance))
          )

        } else {

          output_list <- list(
            estimate = as.matrix(output_estimate),
            variance = as.matrix(output_variance)
          )

        }

        # return
        rout <- output_list
        return(rout)

      }

      # clean output ----
      network_output <- output_manipulator(x = network_output)

      # conformal prediction ----
      conformal_prediction <-
        function(estimate, variance, conformity, interval) {

          lower <- estimate - (variance * conformity)
          condition <- lower < interval[1]
          lower[condition] <- interval[1]

          upper <- estimate + (variance * conformity)
          condition <- upper > interval[2]
          upper[condition] <- interval[2]

          predicted <- unname(cbind(estimate, lower, upper))

          # return ----
          rout <- predicted
          return(rout)

        }

      estimate <- network_output$estimate
      variance <- network_output$variance

      condition <- alpha == object$alpha
      if(condition) {

        conformity_factor <- object$conformity$factor

      } else {

        conformity_score <- object$conformity$score
        conformity_factor <- compute_descriptor(
          x = conformity_score, fun = quantile, probs = 1 - alpha
        )

      }

      condition <- noutput > 1
      if(condition) {

        predicted <- lapply(seq_len(noutput), function(i) {

          conformal_prediction(
            estimate = estimate[, i],
            variance = variance[, i],
            conformity = conformity_factor[i],
            interval = interval[, i]
          )

        })
        names(predicted) <- colnames(y)


      } else {

        predicted <- conformal_prediction(
          estimate = estimate,
          variance = variance,
          conformity = conformity_factor,
          interval = interval
        )

      }

      # return ----
      rout <- predicted
      return(rout)

    }

  }

}

#' Print method for Generalized Regression Neural Network
#'
#' @author David Navega
#' @export
#'
#' @param x a \emph{grnnet} object
#' @param digits digitis to print
#' @param ... ...
#'
print.grnnet <- function(x, digits = 2, ...) {

  cat("Generalized Regression Neural Network\n")
  cat("\nInput(s):  ", NCOL(x$data$x), "\n")
  cat("Output(s): ", NCOL(x$data$y), "\n")
  cat("Sigma:", round(x$sigma, digits = digits))

}


