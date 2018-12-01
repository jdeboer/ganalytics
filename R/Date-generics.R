#' DateRange.
#'
#' Get the date range.
#'
#' @param object The start date of the date range or a object to coerce to a
#'   date range. Alternatively, a query object to replace the date range of.
#' @param endDate The end date of the date range. Alternatively, if
#'   \code{object} is a '.query' object, then endDate is the replacement date
#'   range.
#'
#' @family date range functions
#'
#' @export
#' @rdname DateRange
setGeneric(
  "DateRange",
  function(object, endDate) {},
  valueClass = "dateRange",
  useAsDefault = FALSE
)

#' DateRange<-.
#'
#' Set the date range.
#'
#' @param value The replacement date range.
#'
#' @family date range functions
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

#' StartDate.
#'
#' Get the start date.
#'
#' @param object Object to get start date of.
#' @param value value to set start date of object to.
#'
#' @family date range functions
#'
#' @export
#' @rdname StartDate
setGeneric(
  "StartDate",
  function(object, value) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' StartDate<-.
#'
#' Set the start date.
#'
#' @family date range functions
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

#' EndDate.
#'
#' Get the end date of the date range.
#'
#' @param object Object to get end date of.
#' @param value value to set end date of object to.
#'
#' @family date range functions
#'
#' @export
#' @rdname EndDate
setGeneric(
  "EndDate",
  function(object, value) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' EndDate<-.
#'
#' Set the endDate of the date range.
#'
#' @family date range functions
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

#' Cohort.
#'
#' Get or define a cohort.
#'
#' @param object to get or set the cohorts of.
#' @param value to set the object's cohorts to.
#' @param type of cohort. Only the default of "FIRST_VISIT_DATE" is currently valid.
#'
#' @family date range functions
#' @export
#' @rdname Cohort
setGeneric(
  "Cohort",
  function(object, value, type) {},
  valueClass = "gaCohort",
  useAsDefault = FALSE
)

#' Cohort<-.
#'
#' Set a cohort of a query.
#'
#' @family date range functions
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

