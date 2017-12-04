\name{monmlp-package}
\alias{monmlp-package}
\alias{monmlp}
\docType{package}
\title{
Monotone Multi-Layer Perceptron Neural Network
}
\description{
The monmlp package implements one and two hidden-layer multi-layer
perceptron neural network (MLP) models. An optional monotone constraint,
which guarantees monotonically increasing behaviour
of model outputs with respect to specified covariates, can be added
to the MLP. The resulting monotone MLP (MONMLP) regression model
is based on Zhang and Zhang (1999).

Early stopping can be combined with bootstrap aggregation to
control overfitting. The model reduces to a standard  MLP
neural network if the monotone constraint is not invoked.

MLP and MONMLP models are fit using the \code{\link{monmlp.fit}} function. 
Predictions from a fitted model are made using the 
\code{\link{monmlp.predict}} function. The \code{\link{gam.style}}
function can be used to investigate fitted covariate/response relationships.
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

## MLP w/ 2 hidden-layers (2 nodes each) and early stopping
w.stp <- monmlp.fit(x = x, y = y, hidden1 = 2, hidden2 = 2,
                    bag = TRUE, iter.max = 500, iter.stopped = 10)
lines(x, attr(w.stp, "y.pred"), col = "orange", lwd = 3)

## MONMLP w/ 2 hidden nodes
w.mon <- monmlp.fit(x = x, y = y, hidden1 = 2, monotone = 1,
                    iter.max = 500)
lines(x, attr(w.mon, "y.pred"), col = "blue", lwd = 3)
}
\details{
\tabular{ll}{
Package: \tab monmlp\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}

\references{
Lang, B., 2005. Monotonic multi-layer perceptron networks as 
universal approximators. In: W. Duch et al. (eds.): ICANN 2005, 
Lecture Notes in Computer Science, 3697:31-37. \cr doi:10.1007/11550907

Minin, A., Velikova, M., Lang, B., and Daniels, H., 2010. Comparison 
of universal approximators incorporating partial monotonicity by 
structure. Neural Networks, 23:471-475. \cr doi:10.1016/j.neunet.2009.09.002

Zhang, H. and Zhang, Z., 1999. Feedforward networks with monotone 
constraints. In: International Joint Conference on Neural Networks, 
vol. 3, p. 1820-1823. doi:10.1109/IJCNN.1999.832655
}
\keyword{ package }
