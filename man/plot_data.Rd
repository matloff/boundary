\name{plot_data}
\alias{plot_data}
\title{3D Visualization of Regression Data, Fitted Regression Line, and Confidence Interval Lines}

\usage{
plot_data(X, y, b, confidence_interval, show_plane = FALSE)
}

\description{
\code{plot_data} creates an interactive 3D plot using the \code{plotly} package. It displays the observed data points, a line corresponding to the transformed regression parameter (\eqn{-b[1]/b[2]}), and additional lines representing the boundaries of a given confidence interval. Optionally, if \code{show_plane} is set to \code{TRUE}, the fitted regression plane is also plotted.
}

\arguments{
  \item{X}{A numeric matrix (or data frame) with at least two columns representing the predictor variables.}
  \item{y}{A numeric vector of response values corresponding to the predictors in \code{X}.}
  \item{b}{A numeric vector of estimated regression coefficients.}
  \item{confidence_interval}{A numeric vector of length two giving the lower and upper bounds of the confidence interval for \eqn{-b[1]/b[2]}.}
  \item{show_plane}{An optional logical flag (default \code{FALSE}). When \code{TRUE}, the regression plane is computed and displayed along with the data points.}
}

\value{
This function does not return a value but prints an interactive 3D plot to the active graphical device.
}

\examples{
## Generate sample data
X <- cbind(runif(100, -100, 100), runif(100, -100, 100))
y <- 100 * X[,1] + 200 * X[,2] + rnorm(100, 0, 10)
b <- estimate_b(X, y)

## Compute a 95\% confidence interval using the delta method
ci <- calculate_confidence_interval_by_delta_method(X, y, b, 0.95)

## Plot the data with the fitted line and confidence interval lines, including the regression plane
plot_data(X, y, b, ci, show_plane = TRUE)
}

\seealso{
  \code{\link{calculate_confidence_interval_by_delta_method}},
  \code{\link{calculate_confidence_interval_by_langrange_multipliers}}
}
