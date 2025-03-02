\name{calculate_confidence_interval_by_delta_method}
\alias{calculate_confidence_interval_by_delta_method}
\title{Compute Confidence Interval Using the Delta Method}

\usage{
calculate_confidence_interval_by_delta_method(X, y, b, confidence)
}

\description{
This function computes a confidence interval for a linear regression parameter at the decision boundary \eqn{y = 0} using the delta method. It calculates an approximate variance for the transformation via a gradient-based approach and uses the normal approximation to derive the interval.
}

\arguments{
  \item{X}{A numeric matrix (or data frame) of predictor variables.}
  \item{y}{A numeric vector of response values.}
  \item{b}{A numeric vector of estimated regression coefficients.}
  \item{confidence}{A numeric value specifying the desired confidence level (e.g., \code{0.95} for a 95\% confidence interval).}
}

\value{
A numeric vector of length two containing the lower and upper bounds of the confidence interval for \eqn{-b[1]/b[2]}.
}

\examples{
## Generate sample data
X <- cbind(runif(100, -100, 100), runif(100, -100, 100))
y <- 100 * X[,1] + 200 * X[,2] + rnorm(100, 0, 10)
b <- estimate_b(X, y)

## Calculate the 95\% confidence interval for -b[1]/b[2]
ci <- calculate_confidence_interval_by_delta_method(X, y, b, 0.95)
print(ci)
}

\seealso{
  \code{\link{plot_data}},
  \code{\link{calculate_confidence_interval_by_langrange_multipliers}}
}
