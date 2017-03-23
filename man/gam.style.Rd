\name{gam.style}
\alias{gam.style}
\title{
GAM-style effects plots for interpreting MONMLP models
}
\description{
GAM-style effects plots provide a graphical means of interpreting
fitted MONMLP predictor/predictor relationships. From Plate et al. (2000):
The effect of the \code{i}th input variable at a particular input point 
\code{Delta.i.x} is the change in \code{f} resulting from changing \code{X1}
to \code{x1} from \code{b1} (the baseline value [...]) while keeping the
other inputs constant. The effects are plotted as short line segments,
centered at (\code{x.i}, \code{Delta.i.x}), where the slope of the segment
is given by the partial derivative. Variables that strongly influence
the function value have a large total vertical range of effects.
Functions without interactions appear as possibly broken straight lines
(linear functions) or curves (nonlinear functions). Interactions show up as
vertical spread at a particular horizontal location, that is, a vertical
scattering of segments. Interactions are present when the effect of
a variable depends on the values of other variables.
}

\usage{
gam.style(x, weights, column, baseline = mean(x[,column]),
         epsilon = 1e-5, seg.len = 0.02, seg.cols = "black",
         plot = TRUE, return.results = FALSE, ...)
}
\arguments{
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictor variables.
}
  \item{weights}{
      list returned by \code{\link{monmlp.fit}}.
}
  \item{column}{
      column of \code{x} for which effects plots should be returned.
}
  \item{baseline}{
      value of \code{x[,column]} to be used as the baseline for calculation of predictor effects; defaults to \code{mean(x[,column])}.
}
  \item{epsilon}{
      step-size used in the finite difference calculation of the partial derivatives.
}
  \item{seg.len}{
      length of effects line segments expressed as a fraction of the range of \code{x[,column]}.
}
  \item{seg.cols}{
      colors of effects line segments.
}
  \item{plot}{
      if \code{TRUE} (the default) then an effects plots for each predictand variable is produced.
}
  \item{return.results}{
      if \code{TRUE} then values of effects and partial derivatives for each predictand variable are returned.
}
  \item{\dots}{
      further arguments to be passed to \code{plot}.
}
}
\seealso{
    \code{\link{monmlp.fit}}, \code{\link{monmlp.predict}}
}
\value{
A list with elements:
  \item{effects}{a matrix of predictor effects.}
  \item{partials}{a matrix of predictor partial derivatives.}
}
\examples{
set.seed(1)
x <- matrix(runif(350*6), ncol=6)
y <- as.matrix(5*sin(10*x[,1]*x[,2]) + 20*(x[,3]-0.5)^2 -
               10*x[,4] + 20*x[,5]*x[,6])

w <- monmlp.fit(x = x, y = y, hidden1 = 4, n.trials = 1,
                iter.max = 500)

for (i in seq(ncol(x))) gam.style(x, weights = w, column = i)
}
\references{
    Cannon, A.J. and I.G. McKendry, 2002. A graphical sensitivity analysis
    for interpreting statistical climate models: Application to Indian
    monsoon rainfall prediction by artificial neural networks and
    multiple linear regression models. International Journal of
    Climatology, 22:1687-1708.
    
    Plate, T., J. Bert, J. Grace, and P. Band, 2000. Visualizing the function
    computed by a feedforward neural network. Neural Computation,
    12(6): 1337-1354.
}
