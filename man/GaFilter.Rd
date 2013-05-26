\name{GaFilter}
\alias{GaFilter}
\title{GaFilter}
\value{
  gaFilter
}
\description{
  Methods for getting and setting a filter
}
\examples{
myQuery <- GaQuery(profileId = 123456789)
myExpr <- GaExpr("pagePath", "~", "thank")
GaFilter(myQuery) <- myExpr
GetGaData(myQuery)
}

