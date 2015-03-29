#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include GaGetCoreReport.R
NULL

setMethod(
  f = "GetGaQueries",
  signature = signature(".gaUrlClasses"),
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
    if (length(object@creds) > 0) {
      creds <- object@creds
    } else {
      creds <- GaCreds(
        userName = object@userName,
        appCreds = object@appCreds,
        cache = object@authFile,
        use_oob = use_oob
      )
    }
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
      attr(df, "sampleSize") <- response$sampleSize
      attr(df, "sampleSpace") <- response$sampleSpace
      return(df)
    }
  )[-1]
  sampled <- any(laply(responses, function(response) {isTRUE(response$sampled)}))
  if (sampled) {
    warning("Contains sampled data.")
  }
  return(data)
})
