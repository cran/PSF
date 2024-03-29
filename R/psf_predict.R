#' Forecasting of univariate time series using a trained PSF model
#'
#' Takes a trained PSF model and the prediction horizon as inputs.
#' @param object The trained PSF model generated by psf() function.
#' @param n.ahead The number of predicted values to be obtained.
#' @param \dots Other parameters will be ignored.
#' @return Vector with the resulting predictions
#' 
#' @importFrom stats kmeans
#' 
#' @export
#' @examples
#' ## Train a PSF model from the univariate time series 'nottem' (package:datasets).
#' p <- psf(nottem)
#'
#' ## Forecast the next 12 values of such time series.
#' pred <- predict(p, n.ahead = 12)
predict.psf <- function (object, n.ahead = 1, ...) {

  # Step 1. Check integrity of data (n.ahead must be multiple of cycle).
  original.n.ahead = n.ahead
  fit = n.ahead %% object$cycle
  if (fit > 0) {
    n.ahead = object$cycle * ceiling(n.ahead / object$cycle)
    warning(paste("Prediction horizon is not multiple of", object$cycle, ". Using", n.ahead, "as prediction horizon!"))
  }

  # Step 2. Call internal function to make predictions.
  res <- psf_predict(object$train_data, object$k, object$w, n.ahead, object$cycle)

  # Step 3. Compose the final result.
  res <- as.vector(t(res))[1:original.n.ahead]
  res <- res * (object$dmax - object$dmin) + object$dmin # denormalize predicted data

  return(res)

}
