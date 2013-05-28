\name{GaExpr}
\alias{GaExpr}
\title{GaExpr
Create an expression.}
\description{
  GaExpr Create an expression.
}
\examples{
\donttest{
  myQuery <- GaQuery(profileId = 123456789)
  source_matches_google <- GaExpr("source", "~", "google")
  GaFilter(myQuery) <- source_matches_google
  GetGaData(myQuery)
}
}

