#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include GaGetCoreReport.R
NULL

setMethod(
  f = "GetGaQueries",
  signature = signature("gaQuery"),
  definition = function(.Object) {
    as(.Object, "matrix")
  }
)

#' GetGaData
#' Execute a ganalytics query.
#' @param query the query to execute.
#' @param .progress progress bar to display. use .progress = "none" to turn off.
#' @return a dataframe
setMethod("GetGaData", "gaQuery", function(
  object,
  creds = NULL,
  .progress = "time",
  use_oob = FALSE,
  addViewId = FALSE
) {
  if (is.null(creds)) {
    creds <- object@creds
  }
  queryParams <- GetGaQueries(object)
  responses <- alply(
    .data = queryParams,
    .margins = 2,
    .fun = GaPaginate,
    maxRequestedRows = GaMaxResults(object),
    creds = creds,
    .progress = .progress
  )
  data <- ldply(
    .data = responses,
    .fun = function(response) {
      df <- response$data
      if(addViewId & nrow(df) >= 1) {
        df <- mutate(df, viewId = response$viewId)
      }
      return(df)
    }
  )[-1]
  attr(data, "sampleSize") <- sum(laply(responses, function(response){as.numeric(response$sampleSize)}))
  attr(data, "sampleSpace") <- sum(laply(responses, function(response){as.numeric(response$sampleSpace)}))
  sampled <- any(laply(responses, function(response) {isTRUE(response$sampled)}))
  if (sampled) {
    warning("Contains sampled data.")
  }
  return(data)
})
