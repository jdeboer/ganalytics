#' @include helper-functions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
NULL

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
#' @export
GaQuery <- function(
  view = NA,
  creds = list(),
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
  if(is.na(view)) {
    view <- GaAccounts(creds = creds)$entities[[1]]
  }
  if (length(creds) == 0 & length(appCreds) == 0 & length(authFile) == 0 & is(view, ".gaResource")) {
    creds <- view$creds
  }
  new("gaQuery",
      profileId = GaProfileId(view),
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
      appCreds = appCreds,
      creds = creds
  )
}

modify_query <- function(
  query, # A single query object
  ids = NA, # A vector of of view IDs
  periods = NA, # A vector of lubridate period objects
  columns = NA, # metrics, dimensions and sorting are inferred by non ambiguious character vector called columns
  filters = NA, # A vector of gaFilter objects
  segments = NA, # A vector of gaSegment objects
  sampling_level = NA,
  max_results = NA,
  user_name = NA,
  app_creds = NA,
  start_date = NA,
  end_date = NA,
  metrics = NA,
  dimensions = NA,
  sort_by = NA
) {
  if (!is.na(ids)) {
    GaProfileId(query) <- ids
  }
  if (!is.na(start_date)) {
    GaStartDate(query) <- start_date
  }
  if (!is.na(end_date)) {
    GaEndDate(query) <- end_date
  }
  if (!is.na(metrics)) {
    GaMetrics(query) <- metrics
  }
  if (!is.na(dimensions)) {
    GaDimensions(query) <- dimensions
  }
  if (!is.na(sort_by)) {
    GaSortBy(query) <- sort_by
  }
  if (!is.na(filters)) {
    GaFilters(query) <- filters
  }
  if (!is.na(segment)) {
    GaSegment(query) <- segment
  }
  if (!is.na(sampling_level)) {
    GaSamplingLevel(query) <- sampling_level
  }
  if (!is.na(max_results)) {
    GaMaxResults(query) <- max_results
  }
  # The following are yet to be implemented
  # user_name
  # app_creds
  # period
  # columns
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