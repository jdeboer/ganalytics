#' @include segment-classes.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod callNextMethod
NULL

#' setSegmentFilterScopeNegation
#'
#' Given a \code{.gaSegmentFilter} object, or an object that can be coerced to a
#' \code{.gaSegmentFilter} object, set the \code{negation} and/or \code{scope}
#' of that \code{.gaSegmentFilter} object as per the non-missing arguments.
#'
#' @param object a \code{.gaSegmentFilter} object, or an object that can be
#'   coerced to a \code{.gaSegmentFilter} object.
#' @param negation optional boolean value of \code{TRUE} or \code{FALSE}.
#' @param scope optional either \code{"sessions"} or \code{"users"}.
#'
#' @return a \code{.gaSegmentFilter} object.
#'
#' @keywords internal
setSegmentFilterScopeNegation <- function(object, negation, scope) {
  object <- as(object, ".gaSegmentFilter")
  if(!missing(scope)) object@scope <- scope
  if(!missing(negation)) object@negation <- negation
  validObject(object)
  object
}

#' select_segment_filters_with_scope
#'
#' Given a Dynamic Segment object, or an object that can be coerced to a
#' \code{gaDynSegment}, returns segment filters within object that are of the
#' specified scope (\code{'sessions'} or \code{'users'}).
#'
#' @param object a Dynamic Segment object, or an object that can be coerced to a
#'   \code{gaDynSegment}.
#' @param scope either \code{"sessions"} or \code{"users"} to specify which
#'   segment filters to return as a dynamic segment subset.
#'
#' @return a \code{gaDynSegment} object.
#'
#' @keywords internal
select_segment_filters_with_scope <- function(object, scope) {
  assert_that(
    length(scope) == 1L,
    scope %in% c("sessions", "users")
  )
  dyn_segment <- as(object, "gaDynSegment")
  matching_filters <- unlist(lapply(dyn_segment, ScopeLevel)) %in% scope
  new("gaDynSegment", dyn_segment[matching_filters])
}

setMethod(
  "initialize",
  signature = "gaDynSegment",
  definition = function(.Object, value, name, ...) {
    .Object <- callNextMethod(.Object, ...)
    if(!missing(value)) {
      .Object@.Data <- value
    }
    if(!missing(name)) {
      .Object@name <- name
    }
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  "initialize",
  signature = "gaSegmentId",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      value <- sub(kGaPrefix, "gaid::", value)
      if (!grepl("^gaid::\\-?[0-9A-Za-z]+$", value)) {
        value <- paste("gaid", value, sep = "::")
      }
      .Object@.Data <- value
      validObject(.Object)
    }
    return(.Object)
  }
)

setMethod(
  "initialize",
  signature = "gaSegmentList",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      segment_names <- names(value)
      .Object@.Data <- lapply(seq_along(value), function(i) {
        value[[i]] <- as(value[[i]], ".gaSegment")
        if(is(value[[i]], "gaDynSegment")) {
          segment_name <- segment_names[i]
          if(is.null(segment_name)) {
            segment_name <- character(0)
          }
          value[[i]]@name <- segment_name
        }
        value[[i]]
      })
      names(.Object) <- segment_names
      validObject(.Object)
    }
    .Object
  }
)

# ---- Include, Exclude ----

#' @describeIn Include Define an include segment filter using the supplied
#'   expression.
setMethod(
  "Include",
  signature = "ANY",
  definition = function(object, ..., scope) {
    negation <- FALSE
    if(missing(scope)) {
      SegmentConditionFilter(object, ..., negation = negation)
    } else {
      SegmentConditionFilter(object, ..., negation = negation, scope = scope)
    }
  }
)

#' @describeIn Exclude Define an exclude segment filter using the supplied
#'   expressions.
setMethod(
  "Exclude",
  signature = "ANY",
  definition = function(object, ..., scope) {
    negation <- TRUE
    if(missing(scope)) {
      SegmentConditionFilter(object, ..., negation = negation)
    } else {
      SegmentConditionFilter(object, ..., negation = negation, scope = scope)
    }
  }
)

# ---- SegmentConditionFilter, DynSegment, IsNegated ----

#' @describeIn SegmentConditionFilter Create a non-sequential segment condition
#'   filter from one or more expressions. All conditions within the filter must
#'   hold true within a single session if applied to a gaDynSegment
#'   scoped at session-level, or to a single hit if scoped at user-level.
setMethod(
  "SegmentConditionFilter",
  signature = "ANY",
  definition = function(object, ..., negation, scope) {
    exprList <- unnest_objects(object, ..., class = "gaSegmentConditionFilter")
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, ".compoundExpr")}))
    if(missing(negation) & missing(scope)) {
      setSegmentFilterScopeNegation(exprList)
    } else if (!missing(negation) & missing(scope)) {
      setSegmentFilterScopeNegation(exprList, negation = negation)
    } else if (missing(negation) & !missing(scope)) {
      setSegmentFilterScopeNegation(exprList, scope = scope)
    } else if (!missing(negation) & !missing(scope)) {
      setSegmentFilterScopeNegation(exprList, negation = negation, scope = scope)
    }
  }
)

#' @describeIn IsNegated Tests whether a segment filter is negated, i.e. used to
#'   define an exclude filter for the segment.
setMethod(
  "IsNegated",
  signature = ".gaSegmentFilter",
  definition = function(object) {
    object@negation
  }
)

#' @describeIn IsNegated Sets whether a segment filter should be negated, i.e.
#'   used as an exclude filter in a segment definition.
setMethod(
  "IsNegated<-",
  signature = c(".gaSegmentFilter", "logical"),
  definition = function(object, value) {
    object@negation <- value
    object
  }
)

#' @describeIn DynSegment Defines a list of filters from one or more
#'   expressions applied using the specified scope.
setMethod(
  "DynSegment",
  signature = "ANY",
  definition = function(object, ..., name) {
    exprList <- list(object, ...)
    nested <- sapply(exprList, is, "gaDynSegment")
    segment_filter_list <- lapply(exprList[!nested], function(expr){
      as(expr, ".gaSegmentFilter")
    })
    nested_segment_filters <- unlist(exprList[nested], recursive = FALSE)
    segment_filter_list <- c(
      segment_filter_list,
      nested_segment_filters
    )
    new("gaDynSegment", segment_filter_list, name = name)
  }
)

#' @describeIn DynSegment Returns itself.
setMethod(
  "DynSegment",
  signature = "gaDynSegment",
  definition = function(object) {
    object
  }
)

# ---- ScopeLevel, ScopeLevel<- ----

#' @describeIn ScopeLevel Returns the scope of the supplied .gaSegmentFilter.
setMethod(
  "ScopeLevel",
  signature = c(".gaSegmentFilter", "missing"),
  definition = function(object) {
    object@scope
  }
)

#' @describeIn ScopeLevel Set the scope level of a .gaSegmentFilter to either
#'   "user" or "session" level.
setMethod(
  "ScopeLevel",
  signature = c(".gaSegmentFilter", "character"),
  definition = function(object, value) {
    object@scope <- value
    validObject(object)
    object
  }
)

#' @describeIn ScopeLevel Set the scope level of a .gaSegmentFilter to either
#'   "user" or "session" level.
setMethod(
  "ScopeLevel<-",
  signature = c(".gaSegmentFilter", "character"),
  definition = function(object, value) {
    object@scope <- value
    validObject(object)
    object
  }
)

#' @describeIn ScopeLevel Set the scope level of a gaDynSegment to either
#'   "user" or "session" level.
setMethod(
  "ScopeLevel<-",
  signature = c("gaDynSegment", "character"),
  definition = function(object, value) {
    object <- lapply(object, function(segmentFilter) {
      ScopeLevel(segmentFilter) <- value
      segmentFilter
    })
    object <- new("gaDynSegment", object)
    validObject(object)
    object
  }
)

# ---- PerSession, PerUser ----

#' @describeIn PerSession Create a session level segment filter list from the
#'   supplied expressions, interpreted as condition filters.
setMethod(
  "PerSession",
  signature = "ANY",
  definition = function(object, ...) {
    SegmentConditionFilter(object, ..., scope = "sessions")
  }
)

#' @describeIn PerSession Create a session-level segment sequence filter from the supplied
#'   sequence expression.
setMethod(
  "PerSession",
  signature = "gaSegmentSequenceStep",
  definition = function(object, ...) {
    Sequence(object, ..., scope = "sessions")
  }
)

#' @describeIn PerSession Create a session-level segment sequence filter from the supplied
#'   sequence expression.
setMethod(
  "PerSession",
  signature = "gaSegmentSequenceFilter",
  definition = function(object, ...) {
    Sequence(object, ..., scope = "sessions")
  }
)

#' @describeIn PerSession Set the scope of the supplied metric condition to
#'   session-level.
setMethod(
  "PerSession",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perSession"
      object
    } else {
      SegmentConditionFilter(object, ..., scope = "sessions")
    }
  }
)

#' @describeIn PerSession Set the scope of the supplied non-standard-evaluation
#'   metric condition to session-level.
setMethod(
  "PerSession",
  signature = "formula",
  definition = function(object, ...) {
    PerSession(Expr(object), ...)
  }
)

#' @describeIn PerUser Create a user-level segment filter list from the supplied
#'   expressions, each interpreted as an include segment filter.
setMethod(
  "PerUser",
  signature = "ANY",
  definition = function(object, ...) {
    SegmentConditionFilter(object, ..., scope = "users")
  }
)

#' @describeIn PerUser Create a user-level segment sequence filter from the supplied
#'   sequence expression.
setMethod(
  "PerUser",
  signature = "gaSegmentSequenceStep",
  definition = function(object, ...) {
    Sequence(object, ..., scope = "users")
  }
)

#' @describeIn PerUser Create a user-level segment sequence filter from the supplied
#'   sequence expression.
setMethod(
  "PerUser",
  signature = "gaSegmentSequenceFilter",
  definition = function(object, ...) {
    Sequence(object, ..., scope = "users")
  }
)

#' @describeIn PerUser Set the scope of the supplied metric condition to
#'   user-level.
setMethod(
  "PerUser",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perUser"
      object
    } else {
      SegmentConditionFilter(object, ..., scope = "users")
    }
  }
)

#' @describeIn PerUser Set the scope of the supplied non-standard-evaluation
#'   metric condition to user-level.
setMethod(
  "PerUser",
  signature = "formula",
  definition = function(object, ...) {
    PerUser(Expr(object), ...)
  }
)

#' @describeIn PerHit Create a single step sequence filter from the supplied
#'   expression.
setMethod(
  "PerHit",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    Sequence(And(object, ...))
  }
)

#' @describeIn PerHit Set the scope of the supplied metric condition to
#'   hit-level.
setMethod(
  "PerHit",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perHit"
      object
    } else {
      Sequence(And(object, ...))
    }
  }
)

#' @describeIn PerHit Set the scope of the supplied non-standard-evaluation
#'   metric condition to hit-level.
setMethod(
  "PerHit",
  signature = "formula",
  definition = function(object, ...) {
    PerHit(Expr(object), ...)
  }
)

#' @describeIn PerProduct Set the scope of the supplied metric condition to
#'   product-level.
setMethod(
  "PerProduct",
  signature = "gaMetExpr",
  definition = function(object) {
    ScopeLevel(object) <- "perProduct"
    object
  }
)

#' @describeIn PerProduct Set the scope of the supplied non-standard-evaluation
#'   metric condition to product-level.
setMethod(
  "PerProduct",
  signature = "formula",
  definition = function(object) {
    PerProduct(Expr(object))
  }
)

# ---- Segment ----

#' @describeIn Segment Interpret the supplied numeric value as a segment ID.
setMethod(
  "Segment",
  signature = "numeric",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment Interpret the supplied character value as a segment ID.
setMethod(
  "Segment",
  signature = "character",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment Create a non-sequential segment using the supplied
#'   expressions.
setMethod(
  "Segment",
  signature = "ANY",
  definition = function(object, ...) {
    dyn_segment_def <- list(object, ...)
    segment_filter_list <- lapply(dyn_segment_def, function(segment_filter_list) {
      as(segment_filter_list, ".gaSegmentFilter")
    })
    new(
      "gaDynSegment",
      segment_filter_list
    )
  }
)

#' @describeIn Segment returns NULL
setMethod(
  "Segment",
  signature = "NULL",
  definition = function(object) {
    new("gaDynSegment", list())
  }
)

#' @describeIn Segment Return the segment ID of the supplied GA Management API
#'   user segment.
setMethod(
  "Segment",
  signature = "gaUserSegment",
  definition = function(object) {
    Segment(object$segmentId)
  }
)

# ---- Segments, Segments<- ----

#' @describeIn Segments Returns itself
setMethod(
  "Segments",
  signature = "gaSegmentList",
  definition = function(object) {
    object
  }
)

#' @describeIn Segments Return the definition of the segment applied to the view.
setMethod(
  "Segments",
  signature = "gaQuery",
  definition = function(object) {
    object@segments
  }
)

#' @describeIn Segments Coerce an object into a segmentList of length 1.
setMethod(
  "Segments",
  signature = "ANY",
  definition = function(object) {
    if(inherits(object, c(
      "gaUserSegment",
      ".gaSegment",
      "character",
      "numeric",
      "gaDynSegment",
      ".compoundExpr"
    ))) {
      new("gaSegmentList", list(segment = Segment(object)))
    } else {
      stopifnot(inherits(object, c("list", "NULL")))
      new("gaSegmentList", object)
    }
  }
)

#' @describeIn Segments Set the segments to be used within a query.
setMethod(
  "Segments<-",
  signature = c("gaQuery", "ANY"),
  definition = function(object, value) {
    # Need to define coercions to .gaSegment from char and numeric
    object@segments <- Segments(value)
    object
  }
)

