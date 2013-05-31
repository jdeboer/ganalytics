#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ColTypes.R
#' @include YesNo2Logical.R
#' @include GaListToDataframe.R
#' @include GaGetCoreReport.R
#' @include GaApiRequest.R
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
#' @return a dataframe
#' @export
GetGaData <- function(query, key = NULL, secret = NULL) {
  oauth_config <- list(
    key = NULL,
    secret = NULL,
    file = query@authFile,
    scope = "https://www.googleapis.com/auth/analytics.readonly",
    base_url = "https://accounts.google.com/o/oauth2",
    authorize = "auth",
    access = "token",
    appname = "GANALYTICS"
  )
  oauth <- do.call(new_oauth, oauth_config)
  maxRequestedRows <- GaMaxResults(query)
  queryURLs <- GetGaUrl(query)
  data.ga <- NULL
  sampled <- FALSE
  for (queryUrl in queryURLs) {
    gaResults <- GaListToDataframe(
      GaGetCoreReport(queryUrl, oauth, startIndex = 1, min(maxRequestedRows, kGaMaxResults))                
    )
    if(gaResults$sampled) sampled <- TRUE
    data.ga <- rbind(
      data.ga,
      gaResults$data
    )
    maxRows <- min(gaResults$totalResults, maxRequestedRows)
    if(maxRows > 1) {
      totalPages <- ceiling(maxRows / kGaMaxResults)
      if(totalPages > 1) {
        for(page in 2:totalPages) {
          startIndex <- kGaMaxResults * (page - 1) + 1
          maxResults <- min(kGaMaxResults, (maxRows - startIndex) + 1)
          gaResults <- GaListToDataframe(
            GaGetCoreReport(queryUrl, oauth, startIndex, maxResults)                
          )
          data.ga <- rbind(
            data.ga,
            gaResults$data
          )
        }
      }
    }
  }
  charCols <- lapply(data.ga, class) == "character"
  data.ga <- ColTypes(df = data.ga, colNames = names(charCols)[charCols], asFun = as.factor)
  if (sampled) warning("Contains sampled data.")
  return(data.ga)
}
