#' DateRange
#'
#' Get the date range.
#'
#' @param object The start date of the date range or a object to coerce to a
#'   date range. Alternatively, a \code{.query} object to replace the date range
#'   of.
#' @param endDate The end date of the date range. Alternatively, if
#'   \code{object} is a \code{.query} object, then \code{endDate} is the
#'   replacement date range.
#'
#' @family date range functions
#'
#' @export
#' @rdname DateRange
setGeneric(
  "DateRange",
  function(object, endDate) {standardGeneric("DateRange")},
  valueClass = "dateRange"
)

#' DateRange<-
#'
#' Set the date range.
#'
#' @param value The replacement date range.
#'
#' @export
#' @rdname DateRange
setGeneric(
  "DateRange<-",
  function(object, value) {
    object <- standardGeneric("DateRange<-")
    validObject(object)
    return(object)
  }
)

#' StartDate
#'
#' Get the start date.
#'
#' @param object Object to get start date of.
#' @param value Value to set start date of object to.
#'
#' @family date range functions
#'
#' @export
#' @rdname StartDate
setGeneric(
  "StartDate",
  function(object, value) {standardGeneric("StartDate")},
  valueClass = "Date"
)

#' StartDate<-
#'
#' Set the start date.
#'
#' @export
#' @rdname StartDate
setGeneric(
  "StartDate<-",
  function(object, value) {
    object <- standardGeneric("StartDate<-")
    validObject(object)
    object
  }
)

#' EndDate
#'
#' Get the end date of the date range.
#'
#' @param object Object to get end date of.
#' @param value Value to set end date of object to.
#'
#' @family date range functions
#'
#' @export
#' @rdname EndDate
setGeneric(
  "EndDate",
  function(object, value) {standardGeneric("EndDate")},
  valueClass = "Date"
)

#' EndDate<-
#'
#' Set the endDate of the date range.
#'
#' @export
#' @rdname EndDate
setGeneric(
  "EndDate<-",
  function(object, value) {
    object <- standardGeneric("EndDate<-")
    validObject(object)
    object
  }
)

#' Cohort
#'
#' Get or define a cohort.
#'
#' @param object The object to get or set the cohorts of, generally a \code{".query"}.
#' @param value A \code{"dateRange"} to set the object's cohort to.
#' @param type The type of cohort. Only the default of \code{"FIRST_VISIT_DATE"}
#'   is currently valid.
#'
#' @family date range functions
#' @export
#' @rdname Cohort
setGeneric(
  "Cohort",
  function(object, value, type) {standardGeneric("Cohort")},
  valueClass = "gaCohort"
)

#' Cohort<-
#'
#' Set a cohort of a query.
#'
#' @export
#' @rdname Cohort
setGeneric(
  "Cohort<-",
  function(object, value) {
    object <- standardGeneric("Cohort<-")
    validObject(object)
    object
  }
)

