\name{montecarloCIP}
\alias{montecarloCIP}
\concept{CIP}
\title{Monte Carlo analysis of CIP}
\description{%
  Function which performs a Monte Carlo analysis of CIP indices}
\usage{%
montecarloCIP(data, year, nboot = 250, control = list())
}
\arguments{%
  \item{data}{data frame of indicators; first column country code, second column year, remaining columns indicators. \code{NA} values are allowed.}
  \item{year}{year selected for the monte carlo study.}
  \item{nboot}{number of Monte Carlo replications. Default: \code{nboot = 250}.}
  \item{control}{control parameters (see *Details*).}
}
\details{%
  The argument \code{control} is a list that can supply any of the following components:

  \describe{%
    \item{\code{method}}{method used to construct CIP, among \code{'four-indicators'}, \code{'six-indicators'} and \code{'eight-indicators'}. 
    Default: \code{method = 'eight-indicators'}.}
    \item{\code{aggregation}}{type of aggregation used, among \code{'linear'} and \code{'geometric'}. 
    Default: \code{aggregation = 'linear'}.}
    \item{\code{normalization}}{type or normalization, among \code{'min-max'}, \code{'z-score'}, and \code{'robust-z-score'}. 
    Default: \code{normalization = min-max}.}
    \item{\code{weights}}{weighting scheme. 
    Default: \code{weights = NULL}, i.e. rely on a predefine \code{method} above.}
    \item{\code{missingindicator}}{action in case of missing value, among \code{'last'}, \code{'none'},\code{'reweight'} and \code{'linear'}.  
    Default: \code{missingindicator = 'last'}.}
    \item{\code{techclass}}{technology classification, among \code{'OCDE'} and \code{'Lall'}. 
    Default: \code{techclass = 'OECD'}.}
    \item{\code{cleaning}}{winsorization of outliers, among \code{'random'}, \code{'none'},  \code{'hampel_twosided'} and \code{'hampel_onesided'}.
    Default: \code{cleaning = 'none'}.}    
  }
  
  Setting a given \code{control} parameter to \code{'random'} will generate random choice.
}
\value{%
  A list with the following components:\cr

  \code{cip}: benchmark CIP object with default \code{control} parameters.\cr

  \code{indexMC}: matrix of CIP values, with countries in rows and Monte Carlo replications in columns.\cr

  \code{rankingMC}: matrix of CIP ranking, with countries in rows and Monte Carlo replications in columns.
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
  # compute CIP for benchmarking
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
