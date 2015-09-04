# Backwards compatibility for deprecated functions

#' GaNot (Deprecated, use Not or ! instead).
#'
#' @param ... arguments passed to \code{Not}
#'
#' @rdname GaNot
#' @export GaNot
GaNot <- function(...){
  .Deprecated("Not")
  Not(...)
}

#' GaOr (Deprecated, use Or or | instead).
#'
#' @param ... arguments passed to \code{Or}
#'
#' @rdname GaOr
#' @export GaOr
GaOr <- function(...){
  .Deprecated("Or")
  Or(...)
}

#' GaAnd (Deprecated, use And or & instead).
#'
#' @param ... arguments passed to \code{And}
#'
#' @rdname GaAnd
#' @export GaAnd
GaAnd <- function(...){
  .Deprecated("And")
  And(...)
}

#' GaStartDate (Deprecated)
#'
#' @param ... arguments passed onto \code{StartDate}
#'
#' @rdname GaStartDate
#' @export GaStartDate
GaStartDate <- function(...){
  .Deprecated("StartDate")
  StartDate(...)
}

#' GaEndDate (Deprecated)
#'
#' @param ... arguments passed onto \code{EndDate}
#'
#' @rdname GaEndDate
#' @export GaEndDate
GaEndDate <- function(...){
  .Deprecated("EndDate")
  EndDate(...)
}

#' GaDateRange (Deprecated)
#'
#' @param ... arguments passed onto \code{DateRange}
#'
#' @rdname GaDateRange
#' @export GaDateRange
GaDateRange <- function(...){
  .Deprecated("DateRange")
  DateRange(...)
}

#' GaStartDate<- (Deprecated)
#'
#' @param value passed onto \code{StartDate}
#'
#' @rdname GaStartDate
#' @export GaStartDate<-
`GaStartDate<-` <- function(..., value){
  .Deprecated("StartDate<-")
  `StartDate<-`(..., value)
}

#' GaEndDate<- (Deprecated)
#'
#' @param value passed onto \code{EndDate}
#'
#' @rdname GaEndDate
#' @export GaEndDate<-
`GaEndDate<-` <- function(..., value){
  .Deprecated("EndDate<-")
  `EndDate<-`(..., value)
}

#' GaDateRange<- (Deprecated)
#'
#' @param value passed onto \code{DateRange}
#'
#' @rdname GaDateRange
#' @export GaDateRange<-
`GaDateRange<-` <- function(..., value){
  .Deprecated("DateRange<-")
  `DateRange<-`(..., value)
}

#' GaSplitDateRange (Deprecated)
#'
#' @param ... arguments passed onto \code{SplitDateRange}
#'
#' @export GaSplitDateRange
GaSplitDateRange <- function(...){
  .Deprecated("SplitDateRange")
  SplitDateRange(...)
}

#' GaSegment (Deprecated).
#'
#' @param ... arguments passed onto \code{Segment}
#'
#' @rdname GaSegment
#' @export GaSegment
GaSegment <- function(...){
  .Deprecated("Segment")
  Segment(...)
}

#' GaSegment<- (Deprecated).
#'
#' @param value passed onto \code{Segment}
#'
#' @rdname GaSegment
#' @export GaSegment<-
`GaSegment<-` <- function(..., value){
  .Deprecated("Segment<-")
  `Segment<-`(..., value)
}

#' GaFilter (Deprecated).
#'
#' @param ... arguments passed onto \code{TableFilter}
#'
#' @rdname GaFilter
#' @export GaFilter
GaFilter <- function(...){
  .Deprecated("TableFilter")
  TableFilter(...)
}

#' @rdname GaFilter
#' @export GaFilter<-
`GaFilter<-` <- function(..., value){
  .Deprecated("TableFilter<-")
  `TableFilter<-`(..., value)
}

#' GaProfileId (Deprecated).
#'
#' @param ... arguments passed onto \code{GaView}
#'
#' @rdname GaProfileId
#' @export GaProfileId
GaProfileId <- GaView

#' @rdname GaProfileId
#' @export GaProfileId<-
`GaProfileId<-` <- function(..., value){
  .Deprecated("GaView<-")
  `GaView<-`(..., value)
}
