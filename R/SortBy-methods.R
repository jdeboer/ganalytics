#' @include var-list-classes.R
#' @include query-classes.R
#' @include init-methods.R
#' @include Var-list-generics.R
#' @include utils.R
#' @importFrom methods setMethod as validObject new
NULL

# -- SortBy ----

#' @describeIn SortBy Coerce a .varList object to a .sortBy child-class.
setMethod(
  f = "SortBy",
  signature = ".varList",
  definition = function(object, desc = logical(0)) {
    vars <- as(object, ".sortBy")
    vars@desc <- desc
    validObject(vars)
    vars
  }
)

#' @describeIn SortBy Returns NULL
setMethod("SortBy", "NULL", function(object) {new(".sortBy")})

#' @describeIn SortBy Return a sortBy object given by the variables named within
#'   a character vector, optionally denoted with - or + to indicate decending or
#'   acending sorting for each variable respectively in order of precednece.
setMethod(
  f = "SortBy",
  signature = "character",
  definition = function(object, ..., desc = logical(0)) {
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
