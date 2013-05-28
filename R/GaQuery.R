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
#' @param maxResults the maximum number of results to return,
#'  up to 1,000,000
#' @export
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
GaQuery <- function(
  profileId,
  authFile = "defaultGA.token",
  startDate = Sys.Date() - 8,
  endDate = Sys.Date() - 1,
  metrics = "ga:visits",
  dimensions = "ga:date",
  sortBy = NULL,
  filters = NULL,
  segment = NULL,
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
      maxResults = maxResults,
      authFile = authFile
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
