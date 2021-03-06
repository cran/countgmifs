% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.countgmifs.R
\name{coef.countgmifs}
\alias{coef.countgmifs}
\title{Extract Model Coefficients.}
\usage{
\method{coef}{countgmifs}(object, model.select = "BIC", ...)
}
\arguments{
\item{object}{an \code{countgmifs} fitted object.}

\item{model.select}{when \code{x} is specified any model along the solution path can be selected. The default is \code{model.select="BIC"} which calculates the predicted values using the coefficients from the model having the lowest BIC. Other options are \code{model.select="AIC"} or any numeric value from the solution path.}

\item{...}{other arguments.}
}
\description{
A generic function which extracts the model coefficients from a fitted model object fit using \code{countgmifs}
}
\seealso{
See Also \code{\link{countgmifs}}, \code{\link{predict.countgmifs}}, \code{\link{summary.countgmifs}}, \code{\link{plot.countgmifs}}
}
\keyword{methods}
