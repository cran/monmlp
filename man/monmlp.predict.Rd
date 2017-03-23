\name{monmlp.predict}
\alias{monmlp.predict}

\title{
Make predictions from a fitted MONMLP model
}
\description{
Make predictions from a fitted MONMLP model or ensemble of MONMLP models.
}
\usage{
monmlp.predict(x, weights)
}

\arguments{
  \item{x}{
	covariate matrix with number of rows equal to the number of samples and number of columns equal to the number of covariates.
}
  \item{weights}{
	list containing MONMLP weight matrices and other parameters from \code{\link{monmlp.fit}}.
}
}
\value{
	a matrix with number of rows equal to the number of samples and number of columns equal to the number of predictand variables. If \code{weights} is from an ensemble of models, the matrix is the ensemble mean and the attribute \code{ensemble} contains a list with predictions for each ensemble member.
}

\examples{
set.seed(123)
x <- as.matrix(seq(-10, 10, length = 100))
y <- logistic(x) + rnorm(100, sd = 0.2)

dev.new()
plot(x, y)
lines(x, logistic(x), lwd = 10, col = "gray")

## Ensemble of MONMLP models w/ 3 hidden nodes
w.mon <- monmlp.fit(x = x, y = y, hidden1 = 3, monotone = 1,
                    n.ensemble = 15, bag = TRUE, iter.max = 500,
                    control = list(trace = 0))
p.mon <- monmlp.predict(x = x, weights = w.mon)

## Plot predictions from ensemble members
matlines(x = x, y = do.call(cbind, attr(p.mon, "ensemble")),
         col = "cyan", lty = 2)

## Plot ensemble mean
lines(x, p.mon, col = "blue", lwd = 3)
}

\seealso{
	\code{\link{monmlp.fit}}
}

