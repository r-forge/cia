\name{sensitivitylinearCIP}
\alias{sensitivitylinearCIP}
\concept{CIP}
\title{Sensitivity analysis of the linear CIP}
\description{%
  Function which compute the marginal and component sensitivity of the linear CIP}
\usage{%
sensitivitylinearCIP(weights, normdata, covmethod = c('standard', 'robust'))
}
\arguments{%
  \item{weights}{vector of weights.}
  \item{normdata}{normalized data frame of indicators corresponding to a single year. \code{NA} values are allowed.. \code{NA} values are allowed.}
  \item{covmethod}{method for computing the covariance. Default: \code{covmethod = 'standard'}.}
}
\details{%
  Sensitivity is defined as Sigma * w / (w' * Sigma * w)^{1/2}.
}
\value{%
  A list with the following components:\cr

  \code{sensitivity}: vector of marginal sensitivities.\cr
  
  \code{component}: vector of component sensitivities.\cr

  \code{Sigma}: covariance matrix of normalized data.
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
  \code{\link{computeCIP}} for computing the CIP and \code{\link{compareCIP}} for comparison of CIPs.
}
\examples{%
  # load data
  data(indicatordata)
  # compute CIP for a given year
  cip = computeCIP(indicatordata, year = 2000)
  # compute sensitivity
  weights = c(1/6,1/6,0,0,0,0,1/6,1/6,0,1/6,1/6)
  out = sensitivitylinearCIP(weights, cip$yearlist[[1]]$normindicators)
  print(out)
}
\keyword{htest}
