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
#' @return a dataframe
#' @export
GetGaData <- function(query) {
  authFile <- query@authFile
  # try and catch  
  print(paste("Token auth file provided: ", authFile))  
  if (authFile=="") {
    message("Authentication token file not provided!")
    stop("Error: Please specify the auth token file's location.")
  }
  maxRequestedRows <- GaMaxResults(query)
  queryURLs <- GetGaUrl(query)
  data.ga <- NULL
  sampled <- FALSE
  for (queryUrl in queryURLs) {
    gaResults <- GaListToDataframe(
      GaGetCoreReport(queryUrl, authFile, startIndex = 1, min(maxRequestedRows, kGaMaxResults))                
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
            GaGetCoreReport(queryUrl, authFile, startIndex, maxResults)                
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
