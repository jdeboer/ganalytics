#' @include query-classes.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ga-api-coerce.R
#' @include GaGetCoreReport.R
#' @importFrom methods setMethod as
#' @importFrom plyr alply ldply mutate
#' @importFrom scales percent
NULL

#' GetGaData Execute a ganalytics query.
#' @param query the query to execute.
#' @param creds the Google APIs Project OAuth 2.0 credentials to use.
#' @param .progress progress bar to display. use .progress = "none" to turn off.
#' @param use_oob httr argument
#' @param addViewId logical value indicating whether to add a viewID column for
#'   when more than one view has been provided.
#' @return a dataframe
setMethod("GetGaData", ".query", function(
  query,
  creds = NULL,
  .progress = "time",
  use_oob = FALSE,
  addViewId = FALSE
) {
  if (is.null(creds)) {
    creds <- query@creds
  }
  if (!missing(use_oob)) {
    warning("Argument 'use_oob' is defunct, please use the GaCreds or GoogleApiCreds functions instead to either supply a creds argument or to set the creds of the supplied query object.", call. = FALSE)
  }
  queryParams <- as(query, "matrix")
  # Need to determine if the query object is a MCF or GA query and tell GaPaginate
  responses <- alply(
    .data = queryParams,
    .margins = 2,
    .fun = GaPaginate,
    maxRequestedRows = MaxResults(query),
    creds = creds,
    queryClass = class(query),
    .progress = .progress
  )
  data <- ldply(
    .data = responses,
    .fun = function(response) {
      df <- response$data
      if (addViewId & nrow(df) >= 1) {
        df <- mutate(df, viewId = response$viewId)
      }
      return(df)
    }
  )[-1]
  attr(data, "sampleSize") <- sum(laply(responses, function(response){as.numeric(response$sampleSize)}))
  attr(data, "sampleSpace") <- sum(laply(responses, function(response){as.numeric(response$sampleSpace)}))
  sampleSize <- attr(data, "sampleSize")
  sampleSpace <- attr(data, "sampleSpace")
  sampled <- any(laply(responses, function(response) {isTRUE(response$sampled)}))
  if (sampled) {
    warning(paste("Contains sampled data: ", sampleSize, "/", sampleSpace, "(", percent(sampleSize/sampleSpace), ")."))
  }
  return(data)
})
