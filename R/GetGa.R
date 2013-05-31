#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ColTypes.R
#' @include YesNo2Logical.R
#' @include GaListToDataframe.R
#' @include GaGetCoreReport.R
#' @include GaApiRequest.R
#' @include GaPaginate.R
NULL

setMethod(
  f = "GetGaUrl",
  signature = signature(".gaUrlClasses"),
  definition = function(.Object) {
    as(.Object, "utf8")
  }
)

#' GetGaData
#' Execute a ganalytics query.
#' @param query the query to execute.
#' @param key optional OAuth client ID.
#'  Default is to use system environment variable "GANALYTICS_CONSUMER_ID".
#' @param secret optional OAuth client secret.
#'  Default is to use system environment variable "GANALYTICS_CONSUMER_SECRET".
#' @param quiet supress messages
#' @param details show detailed messages
#' @param .progress progress bar to display. use .progress = "none" to turn off.
#' @return a dataframe
#' @export
GetGaData <- function(
  query,
  key = NULL,
  secret = NULL,
  quiet = FALSE,
  details = FALSE,
  .progress = "time"
) {
  oauth <- new_oauth(
    key = NULL,
    secret = NULL,
    file = query@authFile,
    scope = "https://www.googleapis.com/auth/analytics.readonly",
    base_url = "https://accounts.google.com/o/oauth2",
    authorize = "auth",
    access = "token",
    appname = "GANALYTICS"
  )
  queryURLs <- GetGaUrl(query)
  responses <- llply(
    .data = queryURLs,
    .fun = GaPaginate,
    maxRequestedRows = GaMaxResults(query),
    oauth = oauth,
    quiet = quiet,
    details = details,
    .progress = .progress
  )
  data <- ldply(
    .data = responses,
    .fun = function(response) {
      response$data
    }
  )
  sampled <- responses[[1]]$sampled
  if (sampled) {
    warning("Contains sampled data.")
  }
  return(data)
}
