#' DateRange.
#'
#' Get the date range.
#'
#' @param object The start date of the date range or a object to coerce to a
#'   date range. Alternatively, a query object to replace the date range of.
#' @param endDate The end date of the date range. Alternatively, if
#'   \code{object} is a '.query' object, then endDate is the replacement date
#'   range.
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
#' @export
#' @rdname DateRange
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
#' @export
#' @rdname DateRange
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
#' @export
#' @rdname DateRange
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
#' @export
#' @rdname DateRange
setGeneric(
  "EndDate<-",
  function(object, value) {
    object <- standardGeneric("EndDate<-")
    validObject(object)
    object
  }
)
