#' @include utils.R
#' @include query-classes.R
#' @include management-api-classes.R
#' @include GaApiRequest.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @importFrom methods new as
NULL

#' GaQuery.
#'
#' Create a ganalytics query object
#'
#' @param view view id to use
#' @param creds authentication credentials object created using GoogleApiCreds()
#' @param startDate start date
#' @param endDate end date
#' @param metrics character vector of metrics
#' @param dimensions character vector of dimensions
#' @param sortBy a sort by object
#' @param filters a filters object
#' @param segments a segment object
#' @param samplingLevel either "DEFAULT", "HIGHER_PRECISION" or "FASTER"
#' @param maxResults the maximum number of results to return,
#' @param profileId Deprecated, use view instead.
#'  up to 1,000,000
#'
#' @export
GaQuery <- function(
  view = NA,
  creds = .creds,
  startDate = Sys.Date() - 8,
  endDate = Sys.Date() - 1,
  metrics = "ga:sessions",
  dimensions = "ga:date",
  sortBy = NULL,
  filters = NULL,
  segments = NULL,
  samplingLevel = "DEFAULT",
  maxResults = kGaMaxResults,
  profileId = NA
) {

#   if (!missing(profileId)) {
#     warning("argument profileId is deprecated; please use view instead.",
#             call. = FALSE)
#     view <- profileId
#   }

  if (missing(profileId)) {
    if (!is(view, ".gaResource")) {
      if (any(is.na(view))) {
        view <- GaAccounts(creds = creds)$entities[[1]]
      }
    }
  } else view <- profileId
  if (missing(creds) & is(view, ".gaResource")) {
    creds <- view$creds
  }
  if (is(creds, "character")) {creds <- GaCreds(cache = creds)}
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
      segments = Segment(segments),
      samplingLevel = samplingLevel,
      maxResults = maxResults,
      creds = creds
  )
}

#' McfQuery.
#'
#' Create a Multi-Channel Funnel Reporting API query object
#'
#' @param view view id to use
#' @param creds authentication credentials object created using GoogleApiCreds()
#' @param startDate start date
#' @param endDate end date
#' @param metrics character vector of metrics
#' @param dimensions character vector of dimensions
#' @param sortBy a sort by object
#' @param filters a filters object
#' @param samplingLevel either "DEFAULT", "HIGHER_PRECISION" or "FASTER"
#' @param maxResults the maximum number of results to return,
#'  up to 1,000,000
#'
#' @export
McfQuery <- function(
  view = NA,
  creds = .creds,
  startDate = Sys.Date() - 8,
  endDate = Sys.Date() - 1,
  metrics = "mcf:totalConversions",
  dimensions = "mcf:nthDay",
  sortBy = NULL,
  filters = NULL,
  samplingLevel = "DEFAULT",
  maxResults = kGaMaxResults
) {
  if (is.na(view[[1]])) {
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

#' RtQuery.
#'
#' Create a Real-Time reporting API query object
#'
#' @param view view id to use
#' @param creds authentication credentials object created using GoogleApiCreds()
#' @param metrics character vector of metrics
#' @param dimensions character vector of dimensions
#' @param sortBy a sort by object
#' @param filters a filters object
#' @param maxResults the maximum number of results to return,
#'  up to 1,000,000
#'
#' @export
RtQuery <- function(
  view = NA,
  creds = .creds,
  metrics = "rt:pageviews",
  dimensions = "rt:minutesAgo",
  sortBy = NULL,
  filters = NULL,
  maxResults = kGaMaxResults
) {
  if (is.na(view[[1]])) {
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
  if (!is.na(segments)) {
    Segment(query) <- segments
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

#' @describeIn MaxResults
setMethod(
  f = "MaxResults",
  signature = ".query",
  definition = function(object) {
    object@maxResults
  }
)

#' @describeIn MaxResults
setMethod(
  f = "MaxResults<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    object@maxResults <- as.numeric(value)
    validObject(object)
    object
  }
)

#' @describeIn SamplingLevel
setMethod(
  f = "SamplingLevel",
  signature = ".standardQuery",
  definition = function(object) {
    object@samplingLevel
  }
)

#' @describeIn SamplingLevel
setMethod(
  f = "SamplingLevel<-",
  signature = c(".standardQuery", "ANY"),
  definition = function(object, value) {
    object@samplingLevel <- as.character(value)
    validObject(object)
    object
  }
)

#' @describeIn SamplingLevel
setMethod(
  f = "SamplingLevel",
  signature = "data.frame",
  definition = function(object) {
    sample_params <- attributes(object)[c("sampleSize", "sampleSpace")]
    sample_params$sampleRate <- sample_params$sampleSize / sample_params$sampleSpace
    sample_params
  }
)
