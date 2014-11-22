#' @include GaApiRequest.R
NULL
GaGetCoreReport <- function(query, creds, startIndex = 1, maxResults = 10000, quiet = FALSE, details = FALSE) {
  request <- "data/ga"
  scope <- "https://www.googleapis.com/auth/analytics.readonly"
  query <- c(
    query,
    "start-index" = startIndex,
    "max-results" = maxResults
  )
  data.ga <- ga_api_request(creds = creds, request = request, scope = scope, queries = query)
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
