\name{calculate_confidence_interval_by_langrange_multipliers}
\alias{calculate_confidence_interval_by_langrange_multipliers}
\title{Compute Confidence Interval Using Lagrange Multipliers}

\usage{
calculate_confidence_interval_by_langrange_multipliers(X, y, b, confidence)
}

\description{
This function calculates the confidence interval for a linear regression parameter at the decision boundary \eqn{y = 0} using an approach based on Lagrange multipliers. It finds the smallest and largest values of the transformed parameter using a constraint based on the covariance matrix and an F-distribution quantile.
}

\arguments{
  \item{X}{A numeric matrix (or data frame) of predictor variables.}
  \item{y}{A numeric vector of response values.}
  \item{b}{A numeric vector of estimated regression coefficients.}
  \item{confidence}{A numeric value specifying the desired confidence level (e.g., \code{0.95} for a 95\% confidence interval).}
}

\value{
A numeric vector of length two with the lower and upper bounds of the confidence interval for \eqn{-b[1]/b[2]}.
}

\examples{
## Generate sample data
X <- cbind(runif(100, -100, 100), runif(100, -100, 100))
y <- 100 * X[,1] + 200 * X[,2] + rnorm(100, 0, 10)
b <- estimate_b(X, y)

## Calculate the 95\% confidence interval using the Lagrange multipliers approach
ci <- calculate_confidence_interval_by_langrange_multipliers(X, y, b, 0.95)
print(ci)
}

\seealso{
  \code{\link{plot_data}},
  \code{\link{calculate_confidence_interval_by_delta_method}}
}
