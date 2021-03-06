% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psf_predict.R
\name{predict.psf}
\alias{predict.psf}
\title{Forecasting of univariate time series using a trained PSF model}
\usage{
\method{predict}{psf}(object, n.ahead = 1, ...)
}
\arguments{
\item{object}{The trained PSF model generated by psf() function.}

\item{n.ahead}{The number of predicted values to be obtained.}

\item{\dots}{Other parameters will be ignored.}
}
\value{
Vector with the resulting predictions
}
\description{
Takes a trained PSF model and the prediction horizon as inputs.
}
\examples{
## Train a PSF model from the univariate time series 'nottem' (package:datasets).
p <- psf(nottem)

## Forecast the next 12 values of such time series.
pred <- predict(p, n.ahead = 12)
}
