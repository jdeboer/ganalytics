#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
#' @include ganalytics-package.R
NULL

# -- GaSortBy ----

setMethod(
  f = "SortBy",
  signature = ".varList",
  definition = function(.Object, desc = logical(0)) {
    vars <- as(.Object, ".sortBy")
    vars@desc <- desc
    validObject(vars)
    vars
  }
)

setMethod("SortBy", "NULL", function(.Object) {new(".sortBy")})

setMethod(
  f = "SortBy",
  signature = "character",
  definition = function(.Object, ..., desc = logical(0)) {
    varsChar <- sapply(ArgList(.Object, ...), as.character)
    vars <- as(varsChar, ".sortBy")
    # For any var prefixed with "-", remove the prefix and set the desc flag to TRUE
    # For any var prefixed with "+", remove the prefix and set the desc flag to FALSE
    desc[grep("^\\+", varsChar)] <- FALSE
    desc[grep("^\\-", varsChar)] <- TRUE
    # Set the length of the desc flags to the same length as the vector of GA variables to sort by.
    if(!is.null(vars)) { 
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

setMethod(
  f = "SortBy",
  signature = "list",
  definition = function(.Object, ..., desc = logical(0)) {
    SortBy(as(ArgList(.Object, ...), ".sortBy"), desc = desc)
  }
)

setMethod(
  f = "SortBy",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    if (missing(value)) {
      as(.Object, ".sortBy")
    } else {
      if(length(value) < 1) {
        value <- NULL
      }
      as(.Object, ".sortBy") <- value
      .Object
    }
  }
)

setMethod(
  f = "SortBy<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    as(.Object, ".sortBy") <- value
    .Object
  }
)

# Backwards compatibility

#'@export GaSortBy
GaSortBy <- SortBy

#'@export GaSortBy<-
`GaSortBy<-` <- `SortBy<-`
