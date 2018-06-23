#' @include globaldata.R
#' @include var-list-classes.R
#' @include table-filter-classes.R
#' @include segment-classes.R
#' @importFrom methods setClass setClassUnion prototype new
#' @importFrom assertthat validate_that noNA
#' @importFrom stringr str_detect
#' @importFrom lubridate interval int_start int_end int_start<- int_end<- int_standardize
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
    validations <- list(
      validate_that(all(object@.Data >= 0), msg = "End date cannot be before start date."),
      validate_that(
        noNA(object),
        start >= as.POSIXct(kGaDateOrigin)
      )
    )
    invalids <- !sapply(validations, function(x) {is.logical(x) && length(x) == 1L && !is.na(x) && x})
    if(any(invalids)) as.character(validations[invalids])
  }
)

# -- GA report requests --
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

setClass(
  "gaPivots",
  contains = "list",
  validity = function(object) {
    all_inherit(object, "gaPivot")
  }
)

setClass(
  "gaReportRequest",
  slots = c(
    dimensions = "gaDimensions",
    metrics = "gaMetrics",
    sortBy = "gaSortBy",
    pivots = "gaPivots",
    filters = "gaFilter"
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

# -- GA query construct ----

#' `.query` class.
#'
#' An S4 class to represent a generalised reporting API query.
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
    maxResults = "numeric",
    creds = "list"
  ),
  prototype = prototype(
    maxResults = kGaMaxResults,
    creds = list()
  ),
  validity = function(object) {
    valid <- validate_that(
      length(object@maxResults) == 1,
      object@maxResults >= 1,
      length(object@metrics) >= 1
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
    valid <- validate_that(length(object@samplingLevel) == 1)
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
