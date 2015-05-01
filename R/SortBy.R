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
  signature = ".sortBy",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "SortBy",
  signature = "NULL",
  definition = function(.Object, ..., desc = logical(0)) {
    new(".sortBy", list(...), desc = desc)
  }
)

setMethod(
  f = "SortBy",
  signature = "character",
  definition = function(.Object, ..., desc = logical(0)) {
    varsChar <- as.character(ArgList(.Object, ...))
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
    vars
  }
)

setMethod(
  f = "SortBy",
  signature = "list",
  definition = function(.Object, ..., desc = logical(0)) {
    SortBy(as.character(.Object), ..., desc = desc)
  }
)

setMethod(
  f = "SortBy",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    if (missing(value)) {
      .Object@sortBy
    } else {
      if(length(value) < 1) {
        value <- NULL
      }
      .Object@sortBy <- SortBy(value)
      validObject(.Object)
      return(.Object)
    }
  }
)

setMethod(
  f = "SortBy<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    if(length(value) < 1) {
      value <- NULL
    }
    .Object@sortBy <- SortBy(value)
    validObject(.Object)
    return(.Object)
  }
)

#'@export GaSortBy
GaSortBy <- SortBy

#'@export GaSortBy<-
`GaSortBy<-` <- `SortBy<-`

setMethod(
  f = "GaSortBy",
  signature = "NULL",
  definition = function(.Object, ..., desc = logical(0)) {
    new("gaSortBy", list(...), desc = desc)
  }
)

