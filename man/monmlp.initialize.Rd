\name{monmlp.initialize}
\alias{monmlp.initialize}
\title{
Initialize a MONMLP weight vector
}
\description{
Uniform random initialization of the weight vector used during fitting of a MONMLP model.
}
\usage{
monmlp.initialize(x, y, hidden1, hidden2, init.weights)
}
\arguments{
  \item{x}{
      covariate matrix with number of rows equal to the number of samples and number of columns equal to the number of covariates.
  }
  \item{y}{
      predictand column matrix with number of rows equal to the number of samples.
  }
  \item{hidden1}{
      number of hidden nodes in the first hidden layer.
  }
  \item{hidden2}{
      number of hidden nodes in the second hidden layer.
  }
  \item{init.weights}{
      vector giving the minimum and maximum allowable values of the random weights.
  }
}
\seealso{
    \code{\link{monmlp.reshape}}
}
