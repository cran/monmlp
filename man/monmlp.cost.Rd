\name{monmlp.cost}
\alias{monmlp.cost}

\title{
Least squares cost function for MONMLP fitting
}
\description{
MONMLP mean squared error cost function with analytical calculation
of its gradient via backpropagation.
}
\usage{
monmlp.cost(weights, x, y, hidden1, hidden2, Th, To, Th.prime,
            To.prime, monotone = NULL)
}

\arguments{
  \item{weights}{
    vector of weights.
}
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
}

\value{
    numeric value giving the mean squared error with associated gradient attached as an attribute.
}

\seealso{
    \code{\link{monmlp.cost}}, \code{\link{monmlp.nlm}}, \code{\link{monmlp.fit}}
}
