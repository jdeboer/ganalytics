#' @include var-list-classes.R
#' @include query-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include utils.R
#' @importFrom methods setMethod as validObject new
NULL

# -- GaSortBy ----

#' @describeIn SortBy
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

#' @describeIn SortBy
setMethod("SortBy", "NULL", function(object) {new(".sortBy")})

#' @describeIn SortBy
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

#' @describeIn SortBy
setMethod(
  f = "SortBy",
  signature = "list",
  definition = function(object, ..., desc = logical(0)) {
    SortBy(as(ArgList(object, ...), ".sortBy"), desc = desc)
  }
)

#' @describeIn SortBy
setMethod(
  f = "SortBy",
  signature = c(".query", "ANY"),
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

#' @describeIn SortBy
setMethod(
  f = "SortBy<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".sortBy") <- value
    object
  }
)
