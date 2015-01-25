#' GaQuery
#' Create a ganalytics query object
#' @param profileId profile id to use
#' @param authFile auth file to save token to
#' @param startDate start date
#' @param endDate end date
#' @param metrics character vector of metrics
#' @param dimensions character vector of dimensions
#' @param sortBy a sort by object
#' @param filters a filters object
#' @param segment a segment object
#' @param samplingLevel either "DEFAULT", "HIGHER_PRECISION" or "FASTER"
#' @param maxResults the maximum number of results to return,
#'  up to 1,000,000
#' @include helper-functions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @export
GaQuery <- function(
  profileId,
  userName = character(0),
  appCreds = character(0),
  authFile = character(0),
  startDate = Sys.Date() - 8,
  endDate = Sys.Date() - 1,
  metrics = "ga:sessions",
  dimensions = "ga:date",
  sortBy = NULL,
  filters = NULL,
  segment = NULL,
  samplingLevel = "DEFAULT",
  maxResults = kGaMaxResults
) {
  new("gaQuery",
      profileId = GaProfileId(profileId),
      dateRange = GaDateRange(
        as.Date(startDate),
        as.Date(endDate)
      ),
      metrics = GaMetrics(metrics),
      dimensions = GaDimensions(dimensions),
      sortBy = GaSortBy(sortBy),
      filters = GaFilter(filters),
      segment = GaSegment(segment),
      samplingLevel = samplingLevel,
      maxResults = maxResults,
      authFile = authFile,
      userName = userName,
      appCreds = appCreds
  )
}

setMethod(
  f = "GaMaxResults",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@maxResults
  }
)

setMethod(
  f = "GaMaxResults<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@maxResults <- as.numeric(value)
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  f = "GaSamplingLevel",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@samplingLevel
  }
)

setMethod(
  f = "GaSamplingLevel<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@samplingLevel <- as.character(value)
    validObject(.Object)
    return(.Object)
  }
)