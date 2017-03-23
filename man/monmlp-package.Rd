\name{monmlp-package}
\alias{monmlp-package}
\alias{monmlp}
\docType{package}
\title{
Monotone Multi-Layer Perceptron Neural Network
}
\description{
The monmlp package implements the monotone multi-layer perceptron 
neural network (MONMLP) regression model following Zhang and Zhang (1999).
The  main feature is the monotone constraint, which guarantees monotonically 
increasing behaviour of model outputs with respect to specified 
covariates. The package also features model architectures with one or 
two hidden layers, analytical calculation of the gradient via 
backpropagation, and optional use of early stopping in conjunction with
bootstrap  aggregation to control overfitting. The model reduces to a
standard  multi-layer perceptron neural network if the monotone constraint
is not invoked.

MONMLP models are fit using the \code{\link{monmlp.fit}} function. 
Predictions from a fitted model are made using the 
\code{\link{monmlp.predict}} function. The \code{\link{gam.style}}
function can be used to investigate fitted predictor/predictand relationships.
}
\details{
\tabular{ll}{
Package: \tab monmlp\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Alex J. Cannon

Maintainer: Alex J. Cannon <acannon@eos.ubc.ca>
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
