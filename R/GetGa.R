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
#' @include GaAuth.R
#' @include GaSplitDateRange.R
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
  credsFile = list(client_id = key, client_secret = secret),
  quiet = FALSE,
  details = FALSE,
  .progress = "time",
  use_oob = FALSE,
  addViewId = FALSE
) {
  appname <- "GANALYTICS"
  scope <- "https://www.googleapis.com/auth/analytics.readonly"
  cache <- query@authFile
  userName <- query@userName
  creds <- list(
    app = app_oauth_creds(
      appname = appname,
      creds = credsFile
    ),
    user = list(
      login = userName,
      cache = cache,
      use_oob = use_oob
    )
  )
  queryURLs <- GetGaUrl(query)
  responses <- llply(
    .data = queryURLs,
    .fun = GaPaginate,
    maxRequestedRows = GaMaxResults(query),
    creds = creds,
    quiet = quiet,
    details = details,
    .progress = .progress
  )
  data <- ldply(
    .data = responses,
    .fun = function(response) {
      df <- response$data
      if(addViewId & nrow(df) >= 1) {
        df <- mutate(df, viewId = response$viewId)
      }
      attr(df, "sampleSize") <- response$sampleSize
      attr(df, "sampleSpace") <- response$sampleSpace
      return(df)
    }
  )
  sampled <- any(laply(responses, function(response) {isTRUE(response$sampled)}))
  if (sampled) {
    warning("Contains sampled data.")
  }
  return(data)
}
