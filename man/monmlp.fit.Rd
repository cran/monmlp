\name{monmlp.fit}
\alias{monmlp.fit}

\title{
Fit one or more MLP or MONMLP models
}
\description{
Fit an individual model or ensemble of MLP or MONMLP regression models
using \code{\link{optimx}} optimization routines to minimize a least
squares cost function. Optional stopped training and bootstrap
aggregation (bagging) can be used to help avoid overfitting.

If invoked, the \code{monotone} argument enforces increasing behaviour
between specified columns of \code{x} and model outputs. In this case, the
\code{exp} function is applied to the relevant weights following
initialization and during optimization; manual adjustment of \code{init.weights}
may be needed.

Note: \code{x} and \code{y} are automatically standardized prior to
fitting and predictions are automatically rescaled by
\code{\link{monmlp.predict}}. This behaviour can be suppressed for
\code{y} by the \code{scale.y} argument.
}
\usage{
monmlp.fit(x, y, hidden1, hidden2 = 0, iter.max = 5000,
           n.trials = 1, n.ensemble = 1, bag = FALSE,
           cases.specified = NULL, iter.stopped = NULL,
           scale.y = TRUE, Th = tansig, To = linear,
           Th.prime = tansig.prime, To.prime = linear.prime,
           monotone = NULL, init.weights = NULL,
           max.exceptions = 10, silent = FALSE, method = "BFGS",
           control = list(trace = 0))
}

\arguments{
  \item{x}{
    covariate matrix with number of rows equal to the number of samples and number of columns equal to the number of covariates.
}
  \item{y}{
    response matrix with number of rows equal to the number of samples and number of columns equal to the number of response variables.
}
  \item{hidden1}{
    number of hidden nodes in the first hidden layer.
}
  \item{hidden2}{
    number of hidden nodes in the second hidden layer.
}
  \item{iter.max}{
    maximum number of iterations of the optimization algorithm.
}
  \item{n.trials}{
    number of repeated trials used to avoid local minima.
}
  \item{n.ensemble}{
    number of ensemble members to fit.
}
  \item{bag}{
    logical variable indicating whether or not bootstrap aggregation (bagging) should be used.
}
  \item{cases.specified}{
    if \code{bag = TRUE}, a list that specifies the bootstrapped cases to be used in each ensemble member.
}
  \item{iter.stopped}{
    if \code{bag = TRUE}, specifies the number of stopped training iterations between calculation of the cost function on the out-of-bootstrap cases.
}
  \item{scale.y}{
    logical determining if columns of the response matrix should be scaled to zero mean and unit variance prior to fitting. Set this to \code{FALSE} if using an output layer transfer function that limits the range of predictions.
}
  \item{Th}{
    hidden layer transfer function.
}
  \item{To}{
    output layer transfer function.
}
  \item{Th.prime}{
    derivative of the hidden layer transfer function.
}
  \item{To.prime}{
    derivative of the output layer transfer function.
}
  \item{monotone}{
    column indices of covariates for which the monotonicity constraint should hold.
}
  \item{init.weights}{
    either a vector giving the minimum and maximum allowable values of the random weights, an initial weight vector, or NULL to calculate based on fan-in.
}
  \item{max.exceptions}{
    maximum number of exceptions of the optimization routine before fitting is terminated with an error.
}
  \item{silent}{
    logical determining if diagnostic messages should be suppressed.
}
  \item{method}{
    \code{\link{optimx}} optimization method.
}
  \item{control}{
    \code{list of \link{optimx}} control parameters.
}
}

\value{
    list containing fitted weight matrices with attributes
    including called values of \code{x}, \code{y}, \code{Th}, \code{To},
    \code{Th.prime}, \code{To.prime}, \code{monotone}, \code{bag},
    \code{iter.max}, and \code{iter.stopped}, along with values of
    covariate/response column means and standard deviations
    (\code{x.center}, \code{x.scale}, \code{y.center},
    \code{y.scale}), out-of-bootstrap cases \code{oob},
    predicted values \code{y.pred}, and, if stopped training is
    switched on, the iteration \code{iter.best} and value of
    the cost function \code{cost.best} that minimized the
    out-of-bootstrap validation error.
}

\examples{
set.seed(123)
x <- as.matrix(seq(-10, 10, length = 100))
y <- logistic(x) + rnorm(100, sd = 0.2)

dev.new()
plot(x, y)
lines(x, logistic(x), lwd = 10, col = "gray")

## MLP w/ 2 hidden nodes
w.mlp <- monmlp.fit(x = x, y = y, hidden1 = 2, iter.max = 500)
lines(x, attr(w.mlp, "y.pred"), col = "red", lwd = 3)

## MLP w/ 2 hidden nodes and stopped training
w.stp <- monmlp.fit(x = x, y = y, hidden1 = 2, bag = TRUE,
                    iter.max = 500, iter.stopped = 10)
lines(x, attr(w.stp, "y.pred"), col = "orange", lwd = 3)

## MONMLP w/ 2 hidden nodes
w.mon <- monmlp.fit(x = x, y = y, hidden1 = 2, monotone = 1,
                    iter.max = 500)
lines(x, attr(w.mon, "y.pred"), col = "blue", lwd = 3)
}

\seealso{
    \code{\link{monmlp.predict}}, \code{\link{gam.style}}
}
