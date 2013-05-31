\name{GetGaData}
\alias{GetGaData}
\title{GetGaData
Execute a ganalytics query.}
\usage{
  GetGaData(query, key = NULL, secret = NULL)
}
\arguments{
  \item{query}{the query to execute.}

  \item{key}{optional OAuth client ID.  Default is to use
  system environment variable "GANALYTICS_CONSUMER_ID".}

  \item{secret}{optional OAuth client secret.  Default is
  to use system environment variable
  "GANALYTICS_CONSUMER_SECRET".}
}
\value{
  a dataframe
}
\description{
  GetGaData Execute a ganalytics query.
}

