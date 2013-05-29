#' @include GaApiRequest.R
NULL

GaGetCoreReport <- function(queryUrl, authFile, startIndex = 1, maxResults = 10000) {
  request <- ""
  queryUrl <- paste(
    queryUrl,
    paste(
      paste("start-index", startIndex, sep = "="),
      paste("max-results", maxResults, sep = "="),
      sep = "&"
    ),
    sep = "&"
  )
  data.ga <- GaApiRequest(reporting.api, request, queryUrl, authFile)
  if (!is.null(data.ga$error)) {
    stop(
      with(
        data = data.ga$error,
        expr = paste("Google Analytics error", code, message, sep=" : ")
      )
    )
  }
  return(data.ga)
}
