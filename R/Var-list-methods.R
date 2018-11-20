#' @include var-list-classes.R
#' @include query-classes.R
#' @include Var-list-generics.R
#' @include utils.R
#' @importFrom methods setMethod as validObject new as<- callNextMethod
NULL

# -- GaMetrics ----

#' @describeIn Metrics Coerce one or more supplied objects to .metrics.
setMethod(
  f = "Metrics",
  signature = "ANY",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".metrics")
  }
)

#' @describeIn Metrics Get the list of metrics for a '.query'.
setMethod("Metrics", ".query", function(object) {object@metrics})

#' @describeIn Metrics Set the metrics for a '.query' object.
setMethod(
  f = "Metrics<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    use_class <- class(object@metrics)
    object@metrics <- as(value, use_class)
    object <- updateSortBy(object)
    validObject(object)
    object
  }
)

# -- Dimensions ----

#' @describeIn Dimensions Coerces the supplied character vector or list into a vector of
#'   Google Analytics dimensions.
#' @export
setMethod(
  f = "Dimensions",
  signature = "ANY",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".dimensions")
  }
)

#' @describeIn Dimensions Returns the dimensions used within the supplied query.
#' @export
setMethod("Dimensions", ".query", function(object) {
  object@dimensions
})

#' @describeIn Dimensions Replace the dimensions of the query.
#' @export
setMethod(
  f = "Dimensions<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    use_class <- class(object@dimensions)
    object@dimensions <- as(value, use_class)
    object <- updateSortBy(object)
    validObject(object)
    object
  }
)

# -- SortBy ----

setMethod(
  f = "initialize",
  signature = ".sortBy",
  definition = function(.Object, value = list(), desc = logical(length(value))) {
    .Object@.Data <- value
    .Object@desc <- desc
    .Object
  }
)

#' @describeIn SortBy Coerce a .varList object to a .sortBy child-class.
setMethod(
  f = "SortBy",
  signature = ".varList",
  definition = function(object, desc, type) {
    vars <- as(object, ".sortBy")
    vars@desc <- desc
    vars@orderType <- type
    validObject(vars)
    vars
  }
)

#' @describeIn SortBy Returns NULL
setMethod("SortBy", "NULL", function(object) {new(".sortBy")})

#' @describeIn SortBy Return a sortBy object given by the variables named within
#'   a character vector, optionally denoted with - or + to indicate descending or
#'   ascending sorting for each variable respectively in order of precedence.
setMethod(
  f = "SortBy",
  signature = "character",
  definition = function(object, ..., desc, type) {
    varsChar <- sapply(ArgList(object, ...), as.character)
    vars <- as(varsChar, ".sortBy")
    # For any var prefixed with "-", remove the prefix and set the desc flag to TRUE
    # For any var prefixed with "+", remove the prefix and set the desc flag to FALSE
    desc[grep("^\\+", varsChar)] <- FALSE
    desc[grep("^\\-", varsChar)] <- TRUE
    # Set the length of the desc flags to the same length as the vector of GA variables to sort by.
    if (!is.null(vars)) {
      length(desc) <- length(vars)
    }
    desc[is.na(desc)] <- vars@desc[is.na(desc)]
    # Set desc to TRUE for all metrics (otherwise FALSE) for missing desc flags.
    tempMetDesc <- sapply(vars, is, ".metVar")
    desc[is.na(desc)] <- tempMetDesc[is.na(desc)]
    vars@desc <- desc
    validObject(vars)
    vars
  }
)


#############\/ Transform to method of SortBy and SortBy<- generic functions
setAs(from = ".query", to = ".sortBy",
      def = function(from, to) {
        from@sortBy
      },
      replace = function(from, value) {
        use_class <- class(from@sortBy)
        from@sortBy <- as(value, use_class)
        validObject(from)
        from
      }
)

#' @describeIn SortBy Replace the sort by argument of a query.
setMethod(
  f = "SortBy",
  signature = c(".query", "ANY"),
  # Need to add an argument for acdending/descending order.
  definition = function(object, value) {
    if (missing(value)) {
      as(object, ".sortBy")
    } else {
      if (length(value) < 1) {
        value <- NULL
      }
      as(object, ".sortBy") <- value
      object
    }
  }
)

#' @describeIn SortBy Replace the sort by argument of a query.
setMethod(
  f = "SortBy<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".sortBy") <- value
    object
  }
)
