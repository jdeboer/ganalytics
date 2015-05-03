#' @include helper-functions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
NULL

#' GaQuery
#' Create a ganalytics query object
#' @param viewId profile id to use
#' @param creds authentication credentials object created using GoogleApiCreds()
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
  creds = GoogleApiCreds(),
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
  if(missing(view)) {
    view <- GaAccounts(creds = creds)$entities[[1]]
  }
  if (missing(creds) & is(view, ".gaResource")) {
    creds <- view$creds
  }
  new("gaQuery",
      viewId = GaView(view),
      dateRange = DateRange(
        as.Date(startDate),
        as.Date(endDate)
      ),
      metrics = as(metrics, "gaMetrics"),
      dimensions = as(dimensions, "gaDimensions"),
      sortBy = as(sortBy, "gaSortBy"),
      filters = as(filters, "gaFilter"),
      segment = GaSegment(segment),
      samplingLevel = samplingLevel,
      maxResults = maxResults,
      creds = creds
  )
}

#' @export
McfQuery <- function(
  view = NA,
  creds = GoogleApiCreds(),
  startDate = Sys.Date() - 8,
  endDate = Sys.Date() - 1,
  metrics = "mcf:totalConversions",
  dimensions = "mcf:nthDay",
  sortBy = NULL,
  filters = NULL,
  samplingLevel = "DEFAULT",
  maxResults = kGaMaxResults
) {
  if(missing(view)) {
    view <- GaAccounts(creds = creds)$entities[[1]]
  }
  if (missing(creds) & is(view, ".gaResource")) {
    creds <- view$creds
  }
  new("mcfQuery",
      viewId = GaView(view),
      dateRange = DateRange(
        as.Date(startDate),
        as.Date(endDate)
      ),
      metrics = as(metrics, "mcfMetrics"),
      dimensions = as(dimensions, "mcfDimensions"),
      sortBy = as(sortBy, "mcfSortBy"),
      filters = as(filters, "mcfFilter"),
      samplingLevel = samplingLevel,
      maxResults = maxResults,
      creds = creds
  )
}

#' @export
RtQuery <- function(
  view = NA,
  creds = GoogleApiCreds(),
  metrics = "rt:activeUsers",
  dimensions = "rt:minutesAgo",
  sortBy = NULL,
  filters = NULL,
  maxResults = kGaMaxResults
) {
  if(missing(view)) {
    view <- GaAccounts(creds = creds)$entities[[1]]
  }
  if (missing(creds) & is(view, ".gaResource")) {
    creds <- view$creds
  }
  new("rtQuery",
      viewId = GaView(view),
      metrics = as(metrics, "rtMetrics"),
      dimensions = as(dimensions, "rtDimensions"),
      sortBy = as(sortBy, "rtSortBy"),
      filters = as(filters, "rtFilter"),
      maxResults = maxResults,
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
  creds = NA,
  start_date = NA,
  end_date = NA,
  metrics = NA,
  dimensions = NA,
  sort_by = NA
) {
  if (!is.na(ids)) {
    GaView(query) <- ids
  }
  if (!is.na(start_date)) {
    StartDate(query) <- start_date
  }
  if (!is.na(end_date)) {
    EndDate(query) <- end_date
  }
  if (!is.na(metrics)) {
    Metrics(query) <- metrics
  }
  if (!is.na(dimensions)) {
    Dimensions(query) <- dimensions
  }
  if (!is.na(sort_by)) {
    SortBy(query) <- sort_by
  }
  if (!is.na(filters)) {
    TableFilter(query) <- filters
  }
  if (!is.na(segment)) {
    GaSegment(query) <- segment
  }
  if (!is.na(sampling_level)) {
    SamplingLevel(query) <- sampling_level
  }
  if (!is.na(max_results)) {
    MaxResults(query) <- max_results
  }
  # The following are yet to be implemented
  # periods
  # columns
  # creds
}

setMethod(
  f = "MaxResults",
  signature = ".query",
  definition = function(.Object) {
    .Object@maxResults
  }
)

setMethod(
  f = "MaxResults<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    .Object@maxResults <- as.numeric(value)
    validObject(.Object)
    .Object
  }
)

setMethod(
  f = "SamplingLevel",
  signature = ".standardQuery",
  definition = function(.Object) {
    .Object@samplingLevel
  }
)

setMethod(
  f = "SamplingLevel<-",
  signature = c(".standardQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@samplingLevel <- as.character(value)
    validObject(.Object)
    .Object
  }
)

setMethod(
  f = "SamplingLevel",
  signature = "data.frame",
  definition = function(.Object) {
    sample_params <- attributes(.Object)[c("sampleSize", "sampleSpace")]
    sample_params$sampleRate <- sample_params$sampleSize / sample_params$sampleSpace
    sample_params
  }
)

# Backwards compatibility
#'@export GaSamplingLevel
GaSamplingLevel <- SamplingLevel
#'@export GaSamplingLevel<-
`GaSamplingLevel<-` <- `SamplingLevel<-`
#'@export MaxResults
GaMaxResults <- MaxResults
#'@export MaxResults<-
`MaxResults<-` <- `MaxResults<-`
