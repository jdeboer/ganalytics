#' @include GaApiRequest.R
NULL

GaGetCoreReport <- function(queryUrl, oauth, startIndex = 1, maxResults = 10000, quiet = FALSE, details = FALSE) {
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
  data.ga <- GaApiRequest(reporting.api, request, queryUrl, oauth, quiet, details)
  #data.ga <- GaListToDataframe(data.ga)
  if (!is.null(data.ga$error)) {
    stop(
      with(
        data = data.ga$error,
        expr = paste("Google Analytics error", code, message, sep=" : ")
      )
    )
  }
  data.ga <- GaListToDataframe(data.ga)
  return(data.ga)
}
