#' Train a PSF model from an univariate time series using the PSF algorithm
#'
#' Takes an univariate time series as input. Optionally, specific internal parameters of the PSF algorithm can be also specified.
#' @param data Input univariate time series, in any format (time series (ts), vector, matrix, list, data frame).
#' @param k The number of clusters, or a vector of candidate values to search for the optimum automatically.
#' @param w The window size, or a vector of candidate values to search for the optimum automatically.
#' @param cycle The number of values that conform a cycle in the time series (e.g. 24 hours per day). Only used when input data is not in time series format.
#' @return An object of class 'psf' with 7 elements:
#' \item{original_series}{Original time series stored to be used internally to build further plots.}
#' \item{train_data}{Adapted and normalized internal time series used to train the PSF model.}
#' \item{k}{Number of clusters used}
#' \item{w}{Window size used}
#' \item{cycle}{Determined cycle for the input time series.}
#' \item{dmin}{Minimum value of the input time series (used to denormalize internally further predictions).}
#' \item{dmax}{Maximum value of the input time series (used to denormalize internally further predictions).}
#' @export
#' @examples
#' ## Train a PSF model from the univariate time series 'nottem' (package:datasets).
#' p <- psf(nottem)
#'
#' ## Train a PSF model from the univariate time series 'sunspots' (package:datasets).
#' p <- psf(sunspots)
psf <- function (data, k = seq(2,10), w = seq(1,10), cycle = 24) {

  # Step 1. Convert input data to internal format (data.table).
  series = convert_datatype(data)
  if (is.ts(data)) cycle = frequency(data)

  # Step 2. Check integrity of data (its size must be multiple of cycle).
  fit = nrow(series) %% cycle
  if (fit > 0) {
    warning(paste("Time series length is not multiple of", cycle, ". Cutting last", fit, "values!"))
    series = series[1:(.N - fit)]
  }

  # Step 3. Normalize data.
  dmin = series[, min(data)]; dmax = series[, max(data)]
  series[, data := (data - dmin) / (dmax - dmin)]

  # Step 4. Reshape data according to the cycle of the time series.
  dataset = as.data.table(t(matrix(series[,data], nrow = cycle)))

  # Step 5. Find optimal number (K) of clusters (or use the value specified by the user).
  if (length(k) > 1)
    k = optimum_k(dataset, k)

  # Step 6. Find optimal window size (W) (or use the value specified by the user).
  if (length(w) > 1)
    w = optimum_w(dataset, k, w, cycle)

  # Step 7. Build the 'psf' object to be returned.
  res = list(original_series = data, train_data = dataset, k = k, w = w, cycle = cycle, dmin = dmin, dmax = dmax)

  # Step 8. Assign class "psf" to returned object.
  class(res) <- "psf"

  return(res)

}

