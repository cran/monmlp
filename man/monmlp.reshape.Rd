\name{monmlp.reshape}
\alias{monmlp.reshape}
\title{
Reshape a MONMLP weight vector
}
\description{
Reshapes a weight vector used during fitting of a MONMLP model into the appropriate weight matrices.
}
\usage{
monmlp.reshape(x, y, weights, hidden1, hidden2)
}
\arguments{
  \item{x}{
      covariate matrix with number of rows equal to the number of samples and number of columns equal to the number of covariates.
  }
  \item{y}{
      predictand matrix with number of rows equal to the number of samples and number of columns equal to the number of predictands.
  }
  \item{weights}{
      weight vector of length returned by \code{\link{monmlp.initialize}}.
  }
  \item{hidden1}{
      number of hidden nodes in the first hidden layer.
  }
  \item{hidden2}{
      number of hidden nodes in the second hidden layer.
  }
}
\seealso{
    \code{\link{monmlp.initialize}}
}
