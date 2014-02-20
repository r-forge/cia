\name{compareCIP}
\alias{compareCIP}
\concept{CIP}
\title{Comparison of CIPs}
\description{%
  Function which compares CIP among countries}
\usage{%
compareCIP(cip, bench = NULL)
}
\arguments{%
  \item{cip}{output of the function \code{computeCIP}.}
  \item{bench}{output of the function \code{computeCIP} for the bechmark. Default to \code{NULL}, i.e. no benchmark used.}
}
\details{%
  - Compute the difference in the values of the CIP between two countries, over time, and on average.
  - Compute the average shift in countries' ranks wrt a benchmark.
  
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
    \item{\code{techclass}}{technology classification, among \code{'OECD'} and \code{'Lall'}. 
    Default: \code{techclass = 'OECD'}.}
    \item{\code{cleaning}}{winsorization of outliers, among \code{'random'}, \code{'none'},  \code{'hampel_twosided'} and \code{'hampel_onesided'}.
    Default: \code{cleaning = 'none'}.}  
  }
}
\value{%
  A list with the following components:\cr

  \code{IndexDifference}: array of size (number of years x number of countries x number of countriers) containing the pairwise country difference
  in the value of the CIP for a given year.\cr

  \code{AvIndexDifference}: matrix of size (number of countries x number of countries) containing the 
  year-average value of \code{MeanIndexDifference}.\cr

  \code{AbsRankDifference}: vector of size (number of years x 1) containing the absolue value of the average shift in countries' rank for a given year. Is \code{NULL} if 
  not benchmark is provided.\cr
  
  \code{AvAbsRankDifference}: year-average value of \code{AbsRankDifference}. Is \code{NULL} if no benchmark is provided.
  
  \code{AvRankCorrelation}: year-average value of correlation between ranks. Is \code{NULL} if no benchmark is provided.  
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
  \code{\link{computeCIP}} for computing the CIP.
}
\examples{%
  # load data
  data(indicatordata)
  # compute CIP for a range of years
  cip = computeCIP(indicatordata, year = 2000:2010)
  out = compareCIP(cip)
  par(mfrow = c(1,1))
  image(out$AvIndexDifference)
  print(out$AvIndexDifference)
  # min/max/average difference
  min(out$AvIndexDifference, na.rm = TRUE)
  max(out$AvIndexDifference, na.rm = TRUE)
  mean(out$AvIndexDifference, na.rm = TRUE)
  # use a benchmark (a given year)
  bench = computeCIP(indicatordata, year = 2000)
  cip = computeCIP(indicatordata, year = 2001:2005) 
  out = compareCIP(cip, bench)
  print(out$AbsRankDifference)
  print(out$AvAbsRankDifference)
  print(out$AvRankCorrelation)  
  
}
\keyword{htest}
