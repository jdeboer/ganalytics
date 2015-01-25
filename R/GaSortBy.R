#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
NULL

# -- GaSortBy ----

setMethod(
  f = "GaSortBy",
  signature = "gaSortBy",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaSortBy",
  signature = "character",
  definition = function(.Object, ..., desc = logical(0)) {
    varChar <- ArgList(.Object, ...)
    vars <- unique(
      lapply(
        X = varChar,
        FUN = function(x) {
          # For any var prefixed with "-", remove the prefix and set the desc flag to TRUE
          # For any var prefixed with "+", remove the prefix and set the desc flag to FALSE
          GaVar(sub("^(\\+|\\-)","",x))
        }
      )
    )
    desc[grep("^\\+", varChar)] <- FALSE
    desc[grep("^\\-", varChar)] <- TRUE
    # Set the length of the desc flags to the same length as the vector of GA variables to sort by.
    if(!is.null(vars)) { 
      length(desc) <- length(vars)
    }
    
    # Set desc to True for all metrics with missing desc flags.
    temp <- sapply(
      X = vars,
      FUN = function(x) {
        class(x) == "gaMetVar"
      }
    )
    desc[is.na(desc)] <- temp[is.na(desc)]
    new(
      Class = "gaSortBy",
      vars,
      desc = desc
    )
  }
)

setMethod(
  f = "GaSortBy",
  signature = "list",
  definition = function(.Object, ..., desc = logical(0)) {
    GaSortBy(as.character(.Object), ..., desc = desc)
  }
)

setMethod(
  f = "GaSortBy",
  signature = "NULL",
  definition = function(.Object, ..., desc = logical(0)) {
    new("gaSortBy", list(...), desc = desc)
  }
)


setMethod(
  f = "GaSortBy",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@sortBy
  }
)

setMethod(
  f = "GaSortBy<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    if(length(value) < 1) {
      value <- NULL
    }
    .Object@sortBy <- GaSortBy(value)
    validObject(.Object)
    return(.Object)
  }
)
