\name{computeCIP}
\alias{computeCIP}
\concept{CIP}
\title{Computation of CIP}
\description{%
  Function which computes CIP indices}
\usage{%
computeCIP(data, year = NULL, control = list())
}
\arguments{%
  \item{data}{data frame of indicators; first column country code, second column year, remaining columns indicators. \code{NA} values are allowed.}
  \item{year}{year(s) of the analysis. Default: \code{year = NULL}, i.e. apply the CIP to all years.}
  \item{control}{control parameters (see *Details*).}
}
\details{%
  The argument \code{control} is a list that can supply any of the following components:

  \describe{%
    \item{\code{method}}{method used to construct CIP, among \code{'four-indicators'}, \code{'six-indicators'}, \code{'eight-indicators'} and 
    \code{'mixed-eight-indicators'}. 
    Default: \code{method = 'mixed-eight-indicators'}.}
    \item{\code{aggregation}}{type of aggregation used, among \code{'linear'} and \code{'geometric'}. 
    Default: \code{aggregation = 'geometric'}.}
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
}
\value{%
  A list with the following components:\cr

  \code{index}: matrix of size (ncountry x nyear) containing the CIP value. \code{NA} indicates no country observation available.\cr

  \code{ranking}: matrix of size (ncountry x nyear) containing the CIP ranking. \code{NA} indicates no country observation available.\cr

  \code{changeinranking}: matrix of size (ncountry x nyear-1) containing the CIP ranking changes. \code{NA} indicates no country observation 
  available. Negative value indicate a drop in the ranking (bad performance) while a positive value indicates a gain the ranking (good performance).\cr
  
  \code{yearlist}: list of size nyear, containing for each year, a list with \code{year}: the year, \code{country}: the countries 
  available for the year, \code{normindicator}: a matrix of normalized indicators for the year, \code{weights}: a matrix of weights for the year.
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
  \code{\link{compareCIP}} for comparing the CIP.
}
\examples{%
  # load data
  data(indicatordata)
  # compute CIP for 2000
  cip = computeCIP(indicatordata, year = 2000)
  # display CIP
  plotCIP(cip)
}
\keyword{htest}
