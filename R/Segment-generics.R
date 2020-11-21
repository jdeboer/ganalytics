#' SegmentConditionFilter
#'
#' Create a new gaSegmentConditionFilter object
#'
#' @param object An expression to be used as a non-sequential segment condition.
#' @param ... Other expressions to be \code{And}ed to the first expression
#'   provided.
#' @param negation Optional logical \code{TRUE} or \code{FALSE} to match
#'   segments where this condition has not been met. Default is \code{FALSE},
#'   i.e. inclusive filter.
#' @param scope Optional scope, \code{"users"} or \code{"sessions"} (default).
#'
#' @return A \code{gaSegmentConditionFilter} object.
#'
#' @examples
#' bounced_sessions <- SegmentConditionFilter(Expr(~bounces > 0))
#' return_shoppers <- SegmentConditionFilter(
#'   Expr(~transactions > 1, metricScope = "perUser"),
#'   scope = "users"
#' )
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "SegmentConditionFilter",
  function(object, ..., negation, scope) {standardGeneric("SegmentConditionFilter")},
  valueClass = "gaSegmentConditionFilter"
)

#' Include
#'
#' Set the negation flag of a segment filter to \code{FALSE}.
#'
#' @param object A segment condition or sequence filter to include.
#' @param ... Additional segment conditions to include.
#' @param scope Optional scope, \code{"users"} or \code{"sessions"} (default).
#'
#' @return A \code{.gaSegmentFilter} object with its negate slot set to
#'   \code{FALSE}.
#'
#' @examples
#' return_shoppers <- Include(
#'   Expr(~transactions > 1, metricScope = "perUser"),
#'   scope = "users"
#' )
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "Include",
  function(object, ..., scope) {standardGeneric("Include")},
  valueClass = ".gaSegmentFilter"
)

#' Exclude
#'
#' Set the negation flag of a segment filter to \code{TRUE}.
#'
#' @inheritParams Include
#'
#' @return A \code{.gaSegmentFilter} object with its negate slot set to
#'   \code{TRUE}.
#'
#' @examples
#' exclude_one_time_shoppers <- Exclude(
#'   Expr(~transactions == 1, metricScope = "perUser"),
#'   scope = "users"
#' )
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "Exclude",
  function(object, ..., scope) {standardGeneric("Exclude")},
  valueClass = ".gaSegmentFilter"
)

#' IsNegated
#'
#' Tests whether a segment filter is negated.
#'
#' @param object An object belonging to the superclass \code{.gaSegmentFilter}.
#'
#' @examples
#' exclude_one_time_shoppers <- Exclude(
#'   Expr(~transactions == 1, metricScope = "perUser"),
#'   scope = "users"
#' )
#' IsNegated(exclude_one_time_shoppers) # TRUE
#'
#' @rdname IsNegated
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "IsNegated",
  function(object) {standardGeneric("IsNegated")},
  valueClass = "logical"
)

#' IsNegated<-
#'
#' Sets the negation flag of a segment filter.
#'
#' @param value The value of the negation slot, either \code{TRUE} or
#'   \code{FALSE}.
#'
#' @rdname IsNegated
#' @export
setGeneric(
  "IsNegated<-",
  function(object, value) {
    object <- standardGeneric("IsNegated<-")
    validObject(object)
    object
  }
)

#' DynSegment
#'
#' Combine one or more segment condition filters and/or sequence filters into a
#' \code{gaDynSegment} that is scoped to either \code{'user'} or
#' \code{'session'} level.
#'
#' Segment filters are either sequential or non-sequential conditions.
#' Sequential and non-sequential conditions can be combined using this function.
#'
#' @param object The first filter to include in the segment definition.
#' @param ... Additional filters to include in the segment definition, if
#'   needed.
#' @param name An optional name given to the dynamic segment.
#'
#' @return A \code{gaDynSegment} object.
#'
#' @examples
#' return_shoppers <- SegmentConditionFilter(
#'   Expr(~transactions > 1, metricScope = "perUser"),
#'   scope = "users"
#' )
#' watched_video_then_purchased <- Sequence(
#'   Expr(~eventCategory == "video") & Expr(~eventAction == "play"),
#'   Later(Expr(~transactions > 0))
#' )
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "DynSegment",
  function(object, ..., name = character(0)) {standardGeneric("DynSegment")},
  valueClass = "gaDynSegment"
)

#' PerProduct
#'
#' Set the scope of a gaMetExpr object to product-level.
#'
#' @param object A \code{gaMetExpr} object to coerce to hit-level
#' @param negation Boolean value indicating whether to negate the condition.
#' @return A \code{gaMetExpr} object.
#'
#' @examples
#' with_products_added_more_than_once <- PerProduct(Expr(~productAddsToCart > 1))
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "PerProduct",
  function(object, negation){standardGeneric("PerProduct")},
  valueClass = "gaSegMetExpr"
)

#' PerHit
#'
#' Set the scope of a \code{gaMetExpr} object to hit-level, or transforms a condition
#' filter to a sequence filter of length one (i.e. a combination of conditions
#' for matching a single hit).
#'
#' @param object A \code{gaMetExpr} object to coerce to hit-level or if multiple
#'   expressions are provided, then the first expression to combine into a
#'   single step sequence filter.
#' @param ... Further expressions to be included in the filter definition if
#'   defining a sequence filter of length one.
#' @param negation Boolean value indicating whether to negate the condition.
#'
#' @return A \code{gaMetExpr} or \code{gaSegmentSequenceFilter}.
#'
#' @examples
#' spent_more_than_100_in_a_transaction <- PerHit(Expr(~transactionRevenue > 100))
#' played_intro_video <- PerHit(
#'   Expr(~eventCategory == "Video") &
#'   Expr(~eventAction == "Play") &
#'   Expr(~eventLabel == "Intro")
#' )
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "PerHit",
  function(object, ..., negation){standardGeneric("PerHit")},
  valueClass = c("gaSegMetExpr", "gaSegmentSequenceFilter")
)

#' PerSession
#'
#' Set the scope of a \code{.gaSegmentFilter} or \code{gaMetExpr} object to
#' session-level.
#'
#' @param object A \code{.gaSegmentFilter} or \code{gaMetExpr} object to coerce
#'   to session-level. Alternatively, an dimension expression or segment filter
#'   to coerce into a session scoped \code{gaDynSegment}.
#' @param ... Other filters to include in the \code{gaDynSegment}.
#' @param negation Boolean value indicating whether to negate the condition.
#'
#' @return A \code{gaMetExpr}, \code{.gaSegmentFilter} or \code{gaDynSegment}.
#'
#' @note To define a \code{gaDynSegment} comprised of a single metric
#'   expression, wrap the metric expression in an \code{Include} or
#'   \code{Exclude} call.
#'
#' @examples
#' spent_more_than_100_in_a_session <- PerSession(Expr(~transactionRevenue > 100))
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "PerSession",
  function(object, ..., negation){standardGeneric("PerSession")},
  valueClass = c("gaDynSegment", ".gaSegmentFilter", "gaSegMetExpr")
)

#' PerUser
#'
#' Set the scope of a \code{.gaSegmentFilter} or \code{gaMetExpr} object to user-level.
#'
#' @param object a \code{.gaSegmentFilter} or \code{gaMetExpr} object to coerce to
#'   user-level. Alternatively, an dimension expression or segment filter to
#'   coerce into a user scoped \code{gaDynSegment}.
#' @param ... Other filters to include in the \code{gaDynSegment}.
#' @param negation Boolean value indicating whether to negate the condition.
#' @return A \code{gaMetExpr}, \code{.gaSegmentFilter} or \code{gaDynSegment}.
#'
#' @note To define a \code{gaDynSegment} comprised of a single metric expression,
#'   wrap the metric expression in an \code{Include} or \code{Exclude} call.
#'
#' @examples
#' spent_more_than_100_per_user <- PerUser(Expr(~transactionRevenue > 100))
#'
#' @family dynamic segment functions
#'
#' @export
setGeneric(
  "PerUser",
  function(object, ..., negation){standardGeneric("PerUser")},
  valueClass = c("gaDynSegment", ".gaSegmentFilter", "gaSegMetExpr")
)

#' Segment
#'
#' Define a segment for use in a query's segment list.
#'
#' @param object A segment or other object that can be coerced into a segment,
#'   including dynamic segments, built-in and/or custom segments by their ID.
#' @param ... Other segment conditions, filters or filter lists to include in
#'   the segment's definition (ANDed)
#' @return An object belonging to the \code{.gaSegment} superclass.
#'
#' @family dynamic segment functions
#'
#' @name Segment
#' @export
setGeneric(
  "Segment",
  function(object, ...) {standardGeneric("Segment")},
  valueClass = ".gaSegment"
)

#' Segments
#'
#' Get the list of segments from the object or coerce the supplied objects into
#' a named list of segments.
#'
#' @param object A query object to get the segment list from or to set the
#'   segment list of.
#' @param ... Alternatively, provide one or more named arguments (segments or
#'   objects that can be coerced into segments) including dynamic segments,
#'   built-in and/or custom segments by their ID.
#' @return A \code{gaSegmentList}
#'
#' @examples
#' my_segments <- Segments(list(
#'   bounces = PerSession(Expr(~bounces != 0)),
#'   conversions = PerUser(Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)),
#'   mobile_or_tablet = Expr(~deviceCategory %in% c("mobile", "tablet")),
#'   multi_session_users = Include(PerUser(Expr(~sessions > 1)), scope = "users"),
#'   new_desktop_users = Expr(~deviceCategory == "desktop") & Expr(~userType == "new")
#' ))
#'
#' @family dynamic segment functions
#'
#' @export
#' @rdname Segments
setGeneric(
  "Segments",
  function(object, ...) {standardGeneric("Segments")},
  valueClass = c("gaSegmentList")
)

#' Segments<-
#'
#' Set the segments of the query object.
#'
#' @param value A named list of segments or a single segment.
#'
#' @examples
#' my_query <- GaQuery(view = "987654321")
#' my_segments_list <- list(
#'   bounces = PerSession(Expr(~bounces != 0)),
#'   conversions = PerUser(Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)),
#'   mobile_or_tablet = Expr(~deviceCategory %in% c("mobile", "tablet")),
#'   multi_session_users = Include(PerUser(Expr(~sessions > 1)), scope = "users"),
#'   new_desktop_users = Expr(~deviceCategory == "desktop") & Expr(~userType == "new")
#' )
#' Segments(my_query) <- my_segments_list
#'
#' @export
#' @rdname Segments
setGeneric(
  "Segments<-",
  function(object, value) {
    object <- standardGeneric("Segments<-")
    validObject(object)
    object
  }
)
