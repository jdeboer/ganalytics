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
  .progress = "time",
  use_oob = FALSE,
  cache = getOption("httr_oauth_cache")
) {
  appname <- "GANALYTICS"
  if (is.null(key)) {
    key <- Sys.getenv(str_c(toupper(appname), "_CONSUMER_ID"))
  }
  if (is.null(secret)) {
    secret <- Sys.getenv(str_c(toupper(appname), "_CONSUMER_SECRET"))
  }
  oauth <- oauth2.0_token(
    endpoint = oauth_endpoints("google"),
    app = oauth_app(
      appname = appname, key = key, secret = secret
    ),
    scope = "https://www.googleapis.com/auth/analytics.readonly",
    use_oob = use_oob,
    cache = cache
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
  sampled <- any(laply(responses, function(response) {response$sampled}))
  if (sampled) {
    warning("Contains sampled data.")
  }
  return(data)
}
