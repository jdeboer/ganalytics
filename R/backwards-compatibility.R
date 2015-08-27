# Backwards compatibility

#' GaNot (Deprecated, use Not or ! instead).
#' @export GaNot
#' @param ... arguments passed to \code{Not}
#' @rdname GaNot
GaNot <- function(...){Not(...)}

#' GaOr (Deprecated, use Or or | instead).
#' @export GaOr
#' @param ... arguments passed to \code{Or}
#' @rdname GaOr
GaOr <- function(...){Or(...)}

#' GaAnd (Deprecated, use And or & instead).
#' @export GaAnd
#' @param ... arguments passed to \code{And}
#' @rdname GaAnd
GaAnd <- function(...){And(...)}

#' GaStartDate (Deprecated)
#' For backwards compatibility
#' @export GaStartDate
#' @rdname GaStartDate
#' @param ... arguments passed onto \code{StartDate}
GaStartDate <- function(...){StartDate(...)}

#' GaEndDate (Deprecated)
#' @export GaEndDate
#' @rdname GaEndDate
#' @param ... arguments passed onto \code{EndDate}
GaEndDate <- function(...){EndDate(...)}

#' GaDateRange (Deprecated)
#' @export GaDateRange
#' @rdname GaDateRange
#' @param ... arguments passed onto \code{DateRange}
GaDateRange <- function(...){DateRange(...)}

#' GaStartDate<- (Deprecated)
#' @export GaStartDate<-
#' @rdname GaStartDate
#' @param value passed onto \code{StartDate}
`GaStartDate<-` <- function(..., value){`StartDate<-`(..., value)}

#' GaEndDate<- (Deprecated)
#' @export GaEndDate<-
#' @rdname GaEndDate
#' @param value passed onto \code{EndDate}
`GaEndDate<-` <- function(..., value){`EndDate<-`(..., value)}

#' GaDateRange<- (Deprecated)
#' @export GaDateRange<-
#' @rdname GaDateRange
#' @param value passed onto \code{DateRange}
`GaDateRange<-` <- function(..., value){`DateRange<-`(..., value)}

#' GaSplitDateRange (Deprecated)
#' @export GaSplitDateRange
#' @param ... arguments passed onto \code{SplitDateRange}
GaSplitDateRange <- function(...){SplitDateRange(...)}

#' GaSegment (Deprecated).
#'
#' @param ... arguments passed onto \code{Segment}
#' @export GaSegment
#' @rdname GaSegment
GaSegment <- function(...){Segment(...)}

#' GaSegment<- (Deprecated).
#'
#' @param value passed onto \code{Segment}
#' @export GaSegment<-
#' @rdname GaSegment
`GaSegment<-` <- function(..., value){`Segment<-`(..., value)}

#' @export GaFilter
#' @rdname TableFilter
GaFilter <- TableFilter

#' @export GaFilter<-
#' @rdname TableFilter
`GaFilter<-` <- `TableFilter<-`

#' @rdname GaView
#' @export GaProfileId
GaProfileId <- GaView

#' @rdname GaView
#' @export GaProfileId<-
`GaProfileId<-` <- `GaView<-`
