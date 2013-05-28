\name{GaQuery}
\alias{GaQuery}
\usage{
  GaQuery(profileId, authFile = "defaultGA.token",
    startDate = Sys.Date() - 8, endDate = Sys.Date() - 1,
    metrics = "ga:visits", dimensions = "ga:date",
    sortBy = NULL, filters = NULL, segment = NULL,
    maxResults = kGaMaxResults)
}
\arguments{
  \item{profileId}{profile id to use}

  \item{authFile}{auth file to save token to}

  \item{startDate}{start date}

  \item{endDate}{end date}

  \item{metrics}{character vector of metrics}

  \item{dimensions}{character vector of dimensions}

  \item{sortBy}{a sort by object}

  \item{filters}{a filters object}

  \item{segment}{a segment object}

  \item{maxResults}{the maximum number of results to
  return, up to 1,000,000}
}

