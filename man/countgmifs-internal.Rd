\name{countgmifs-internal}
\alias{countgmifs-internal}
\alias{hilbe}
\alias{nb.theta}
\alias{poisson.theta}

\title{
Functions Called by countgmifs Functions, Not by the User
}
\description{
These functions are called my other \code{countgmifs} functions and are not intended to be directly called by the user.
}
\details{
The \code{nb.theta} and \code{poisson.theta} functions for the negative binomial and poisson regression models, respectively, are the log-likelihood functions and are used to estimate the intercept non-penalized subset parameters (if included) at each step of the algorithm.
The \code{hilbe} function Hilbes estimate of the alpha parameter for the negative binomial regression model.
}
\author{
Kellie J. Archer, \email{archer.43@osu.edu}
}
\seealso{
See Also as \code{ \link{countgmifs} }
}
\keyword{ internal }