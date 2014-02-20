\name{plotCIP}
\alias{plotCIP}
\concept{CIP}
\title{Plot CIP}
\description{%
  Function which plots CIP and Monte Carlo results}
\usage{%
plotCIP(cip, type = c('index', 'ranking'), mc = NULL, ...)
}
\arguments{%
  \item{cip}{output of the function \code{computeCIP}.}
  \item{type}{type of plot, among \code{'index'} and \code{'ranking'}. Default: \code{type = 'index'}.}
  \item{mc}{Monte Carlo object of the function \code{montecarloCIP}. Default: \code{mc = NULL}, i.e. no plot of Monte Carlo analysis.}
  \item{...}{additional graphical parameters.}
}
\value{%
  Display a plot of CIP and Monte Carlo results.
}
\note{%
  Please cite the package in publications. Use \code{citation("CIP")}.
}
\author{%
  Kris Boudt and David Ardia.
}
\references{%
  Saisana, M., Saltelli, A., Tarantola, S. (2005)
  Uncertainty and sensitivity analysis techniques as tools for the quality assessment
  of composite indicators.
  \emph{Journal of Royal Statistical Society} \bold{168}, pp.307-323.
}
\seealso{%
  \code{\link{computeCIP}} for computing the CIP and \code{\link{montecarloCIP}} for Monte Carlo study of CIPs.
}
\examples{%
  # load data
  data(indicatordata)
  # compute CIP for year 2000
  cip = computeCIP(indicatordata, year = 2000)
  # MC settings
  control = list(method = 'random', aggregation = 'random')
  # run MC
  set.seed(1234)
  mc = montecarloCIP(indicatordata, year = 2000, control = control)
  # graphical analysis
  par(mfrow = c(1,1))
  plotCIP(cip, type = 'index', mc)
}
\keyword{htest}
