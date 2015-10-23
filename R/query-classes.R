#' @include globaldata.R
#' @include var-list-classes.R
#' @include table-filter-classes.R
#' @include segment-classes.R
#' @importFrom methods setClass setClassUnion prototype new
#' @importFrom assertthat validate_that
#' @importFrom stringr str_detect
NULL

# ---- dateRange ----

# Consider changing the definition of dateRange to be inheriting from or
# extending a lubridate interval class. This would require rounding the start
# and end to the nearest day, possibly checking that the timezone is aligned
# with the view it is being applied to (or forcing the timezone to be UTC or the
# system's local timezone). Also making sure that the interval is positive, i.e
# the start is not after the end.

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
  slots = c(
    startDate = "Date",
    endDate = "Date"
  ),
  prototype = prototype(
    startDate = Sys.Date() - 8,
    endDate = Sys.Date() - 1
  ),
  validity = function(object) {
    if (length(object@startDate) != length(object@endDate)) {
      "startDate and endDate must be the same length"
    } else if (all(object@startDate > object@endDate)) {
      "endDate cannot be before startDate"
    } else if (all(object@startDate < kGaDateOrigin)) {
      paste("Start date cannot preceed Google Analytics launch date:", kGaDateOrigin)
    } else TRUE
  }
)

# ---- View ID ----

#' `viewId` class.
#'
#' An S4 class to represent a Google Analytics view's ID.
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
      "viewId must be an string of digits preceeded by 'ga:'"
    }
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
    segments = ".gaSegment",
    buckets = "numeric"
  ),
  prototype = prototype(
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy")
  ),
  contains = ".standardQuery"
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
