\name{monmlp.nlm}
\alias{monmlp.nlm}

\title{
Fit MONMLP model via nlm optimization function
}
\description{
Helper function used to fit a MONMLP model via the \code{\link{nlm}} routine.
}
\usage{
monmlp.nlm(x, y, hidden1, hidden2 = 0, iter.max = 5000,
           n.trials = 1, Th = tansig, To = linear,
           Th.prime = tansig.prime, To.prime = linear.prime,
           monotone = NULL, init.weights = c(-0.5, 0.5),
           max.exceptions = 10, silent = FALSE, ...)
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
    a list containing elements
    \item{weights}{final weight vector}
    \item{cost}{final value of the cost function}
    \item{code}{termination code from \code{\link{nlm}}}
}

\seealso{
    \code{\link{monmlp.fit}}
}
