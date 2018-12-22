#' @include globaldata.R
#' @include var-list-classes.R
#' @include table-filter-classes.R
#' @include segment-classes.R
#' @importFrom methods setClass setClassUnion prototype new
#' @importFrom assertthat validate_that noNA
#' @importFrom stringr str_detect
#' @importFrom lubridate interval int_start int_end int_start<- int_end<- int_standardize int_length
NULL

# ---- dateRange ----

#' `dateRange` class.
#'
#' An S4 class to represent a date range.
#'
#' @rdname dateRange-class
#' @keywords internal
#'
#' @export
setClass(
  "dateRange",
  contains = getClass("Interval", where = "lubridate"),
  prototype = prototype(
    as.numeric(as.POSIXct(Sys.Date() - 2L)) - as.numeric(as.POSIXct(Sys.Date() - 8L)),
    start = as.POSIXct(Sys.Date() - 8L),
    tzone = "UTC"
  ),
  validity = function(object) {
    validations <- list(
      validate_that(all(int_length(object) >= 0), msg = "End date cannot be before start date."),
      validate_that(
        noNA(object),
        all(object@start >= as.POSIXct(kGaDateOrigin))
      )
    )
    invalids <- !sapply(validations, function(x) {is.logical(x) && length(x) == 1L && !is.na(x) && x})
    if(any(invalids)) as.character(validations[invalids])
  }
)

# -- GA report requests --

#' An S4 class to represent a cohort of users over a specific date range
#'
#' @slot type A length-one character vector representing the type of cohort. At
#'   this time only FIRST_VISIT_DATE is supported by the Google Analytics
#'   Reporting API.
#' @keywords internal
setClass(
  "gaCohort",
  contains = "dateRange",
  slots = c(
    type = "character"
  ),
  prototype = prototype(
    type = "FIRST_VISIT_DATE"
  ),
  validity = function(object) {
    validate_that(
      all(type %in% c("FIRST_VISIT_DATE"))
    )
  }
)

#' An S4 class to represent a pivot report configuration
#'
#' @slot dimensions A list of dimensions to pivot into columns.
#' @slot dimensionFilters A filter for one or more of the dimensions.
#' @slot metrics A list of metrics to aggregate.
#' @slot startGroup The starting index of the group of dimension values to pivot.
#' @slot maxGroupCount The maximum number of dimension value groups to pivot over.
#'
#' @keywords internal
setClass(
  "gaPivot",
  slots = c(
    dimensions = "gaDimensions",
    dimensionFilters = "gaDimFilter",
    metrics = "gaMetrics",
    startGroup = "integer",
    maxGroupCount = "integer"
  ),
  prototype = prototype(
    dimensions = new("gaDimensions", list()),
    dimensionFilters = new("gaDimFilter"),
    metrics = new("gaMetrics", list()),
    startGroup = 1L,
    maxGroupCount = 5L
  )
)

# ---- View ID ----

#' `viewId` class.
#'
#' An S4 class to represent a Google Analytics view ID.
#'
#' @rdname viewId-class
#' @keywords internal
#'
#' @export
setClass(
  "viewId",
  contains = "character",
  validity = function(object) {
    if (all(str_detect(object, "^ga:[0-9]+$"))) {
      TRUE
    } else {
      "viewId must be an string of digits preceded by 'ga:'"
    }
  }
)

setClass(
  "gaReportRequest",
  slots = c(
    dimensions = "gaDimensions",
    metrics = "gaMetrics",
    sortBy = "gaSortBy",
    pivot = "gaPivot",
    tableFilter = "gaFilter"
  )
)

setClass(
  "gaReportRequests",
  contains = "list",
  slots = c(
    viewId = "viewId",
    creds = "list",
    dateRanges = "dateRange",
    samplingLevel = "character",
    segments = "gaSegmentList",
    cohorts = "gaCohort"
  ),
  prototype = prototype(
    creds = list(),
    dateRanges = new("dateRange"),
    samplingLevel = "DEFAULT"
  ),
  validity = function(object) {
    all_inherit(object, "gaReportRequest")
  }
)

setClass("ga4_req")

# -- GA query construct ----

#' `.query` class.
#'
#' An S4 class to represent a generalised reporting API query.
#'
#' @slot viewId The view ID of the query, as a viewId object
#' @slot metrics The metrics to query, an object belonging to the .metrics superclass.
#' @slot dimensions The dimensions of the query, an object belonging to the .dimensions superclass.
#' @slot sortBy The order and direction of metrics/dimensions to sort by, an object belonging to the .sortBy superclass.
#' @slot filters Dimension or metrics conditions to filter the rows by, an object belonging to the .tableFilter superclass.
#' @slot maxResults An interger giving the maximum number of rows to return.
#' @slot creds The API credentials to use for the query.
#'
#' @rdname query-class
#' @keywords internal
#'
#' @export
setClass(
  ".query",
  slots = c(
    viewId = "viewId",
    metrics = ".metrics",
    dimensions = ".dimensions",
    sortBy = ".sortBy",
    filters = ".tableFilter",
    maxResults = "integer",
    creds = "list"
  ),
  prototype = prototype(
    maxResults = kGaMaxResults,
    creds = list()
  ),
  validity = function(object) {
    valid <- validate_that(
      length(object@maxResults) == 1L,
      object@maxResults >= 1L,
      length(object@metrics) >= 1L
    )
    if (valid == TRUE) {
      if (object@maxResults > kGaMaxRows) {
        "maxResults cannot be greater than 1,000,000"
      } else if (!all(object@sortBy %in% union(object@metrics, object@dimensions))) {
        "sortBy must contain varNames also used as metrics and/or dimensions"
      } else TRUE
    } else valid
  }
)

#' `.standardQuery` class.
#'
#' An S4 class to represent a standard reporting API query.
#'
#' @slot dateRange The date range for the query as a dateRange object
#' @slot samplingLevel The sampling level of the query as a character string.
#'
#' @rdname standardQuery-class
#' @keywords internal
#'
#' @export
setClass(
  ".standardQuery",
  slots = c(
    dateRange = "dateRange",
    samplingLevel = "character"
  ),
  prototype = prototype(
    dateRange = new("dateRange"),
    samplingLevel = "DEFAULT"
  ),
  contains = ".query",
  validity = function(object) {
    valid <- validate_that(length(object@samplingLevel) == 1L)
    if (valid == TRUE) {
      if (!(object@samplingLevel %in% samplingLevel_levels)) {
        paste("samplingLevel must be one of:", paste(samplingLevel_levels, collapse = ", "))
      } else TRUE
    } else valid
  }
)

#' `gaQuery` class.
#'
#' An S4 class to represent a Core Reporting API query.
#'
#' @slot segments A gaSegmentList giving one or more segments to query.
#' @slot buckets A numeric vector of breakpoints for histogram buckets.
#' @slot lifetimeValue A logical flag of whether to perform life-time value analysis.
#'
#' @rdname gaQuery-class
#' @keywords internal
#'
#' @export
setClass(
  "gaQuery",
  slots = c(
    metrics = "gaMetrics",
    dimensions = "gaDimensions",
    sortBy = "gaSortBy",
    filters = "gaFilter",
    segments = "gaSegmentList",
    buckets = "numeric",
    lifetimeValue = "logical"
  ),
  prototype = prototype(
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy"),
    lifetimeValue = FALSE
  ),
  contains = ".standardQuery",
  validity = function(object) {
    validate_that(
      length(object@lifetimeValue) == 1L,
      object@lifetimeValue %in% c(TRUE, FALSE)
    )
  }
)

#' `mcfQuery` class.
#'
#' An S4 class to represent a Multi-Channel Funnel Reporting API query.
#'
#' @rdname mcfQuery-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfQuery",
  slots = c(
    metrics = "mcfMetrics",
    dimensions = "mcfDimensions",
    sortBy = "mcfSortBy",
    filters = "mcfFilter"
  ),
  prototype = prototype(
    metrics = new("mcfMetrics"),
    dimensions = new("mcfDimensions"),
    sortBy = new("mcfSortBy")
  ),
  contains = ".standardQuery"
)

#' `rtQuery` class.
#'
#' An S4 class to represent a Real-Time Reporting API query.
#'
#' @rdname rtQuery-class
#' @keywords internal
#'
#' @export
setClass(
  "rtQuery",
  slots = c(
    metrics = "rtMetrics",
    dimensions = "rtDimensions",
    sortBy = "rtSortBy",
    filters = "rtFilter"
  ),
  prototype = prototype(
    metrics = new("rtMetrics"),
    dimensions = new("rtDimensions"),
    sortBy = new("rtSortBy")
  ),
  contains = ".query"
)
