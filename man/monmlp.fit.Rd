\name{monmlp.fit}
\alias{monmlp.fit}

\title{
Fit a MONMLP model or an ensemble of MONMLP models
}
\description{
Fit a MONMLP model or an ensemble of MONMLP models using the 
\code{\link{nlm}} optimization routine. Optional stopped training 
and bootstrap aggregation (bagging) can be used to help avoid 
overfitting. 

The default monotonicity constraint enforced by the \code{monotone} 
argument is for increasing behaviour. Monotonic decreases can be 
forced by multiplying the appropriate columns of \code{x} by 
\code{-1}.

Note: \code{x} and \code{y} are automatically standardized prior to 
fitting and predictions are automatically rescaled by 
\code{\link{monmlp.predict}}. This behaviour can be suppressed for 
\code{y} by the \code{scale.y} argument.
}
\usage{
monmlp.fit(x, y, hidden1, hidden2 = 0, iter.max = 5000,
           n.trials = 1, n.ensemble = 1, bag = FALSE,
           iter.stopped = NULL, scale.y = TRUE, Th = tansig,
           To = linear, Th.prime = tansig.prime,
           To.prime = linear.prime, monotone = NULL,
           init.weights = c(-0.5, 0.5), max.exceptions = 10,
           silent = FALSE, ...)
}

\arguments{
  \item{x}{
	covariate matrix with number of rows equal to the number of samples and number of columns equal to the number of covariates.
}
  \item{y}{
	predictand matrix with number of rows equal to the number of samples and number of columns equal to the number of predictands.
}
  \item{hidden1}{
	number of hidden nodes in the first hidden layer.
}
  \item{hidden2}{
	number of hidden nodes in the second hidden layer.
}
  \item{iter.max}{
    maximum number of iterations of the \code{\link{nlm}} optimization algorithm.
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
  \item{iter.stopped}{
    if \code{bag = TRUE}, specifies the number of stopped training iterations between calculation of the cost function on the out-of-bootstrap cases.
}
  \item{scale.y}{
    logical determining if columns of the predictand matrix should be scaled to zero mean and unit variance prior to fitting. Set this to \code{FALSE} if using an output layer transfer function that limits the range of predictions.
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
    either a vector giving the minimum and maximum allowable values of the random weights or an initial weight vector.
}
  \item{max.exceptions}{
    maximum number of exceptions of the \code{\link{nlm}} routine before fitting is terminated with an error.
}
  \item{silent}{
    logical determining if diagnostic messages should be suppressed.
}
  \item{\dots}{
    additional parameters passed to the \code{\link{nlm}} optimization routine.
}
}

\value{
    list containing fitted weight matrices with attributes
    including called values of \code{x}, \code{y}, \code{Th}, \code{To},
    \code{monotone}, \code{bag}, and \code{iter.stopped}, along with
    values of covariate/predictand column means and standard
    deviations (\code{x.center}, \code{x.scale}, \code{y.center},
    \code{y.scale}), out-of-bootstrap cases \code{oob}, and
    predicted values \code{y.pred}.
}

\examples{
set.seed(123)
x <- as.matrix(seq(-10, 10, length = 100))
y <- logistic(x) + rnorm(100, sd = 0.2)

plot(x, y)
lines(x, logistic(x), lwd = 10, col = "gray")

## MLP w/ 2 hidden nodes
w.mlp <- monmlp.fit(x = x, y = y, hidden1 = 2)
lines(x, attr(w.mlp, "y.pred"), col = "red", lwd = 3)

## MLP w/ 2 hidden nodes and stopped training
w.stp <- monmlp.fit(x = x, y = y, hidden1 = 2, bag = TRUE,
                    iter.stopped = 50)
lines(x, attr(w.stp, "y.pred"), col = "orange", lwd = 3)

## MONMLP w/ 2 hidden nodes
w.mon <- monmlp.fit(x = x, y = y, hidden1 = 2, monotone = 1)
lines(x, attr(w.mon, "y.pred"), col = "blue", lwd = 3)
}

\seealso{
    \code{\link{monmlp.predict}}, \code{\link{monmlp.nlm}}, \code{\link{monmlp.cost}},
    \code{\link{gam.style}}
}
