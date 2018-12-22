#' @include var-classes.R
#' @include var-coerce.R
#' @include expr-classes.R
#' @include var-list-classes.R
#' @include var-list-coerce.R
#' @include query-classes.R
#' @include Var-generics.R
#' @include Var-list-generics.R
#' @include utils.R
#' @importFrom methods new setMethod as<- callNextMethod
NULL

setMethod(
  f = "initialize",
  signature = ".gaVar",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      tmp <- as.character(tolower(value))
      ## Substitute ga; ga- ga. ga_ with ga:
      ## Check var name starts with ga:
      ## Put ga: at start of GA var name
      tmp <- sub(kGaPrefix, "ga:", tmp)
      tmp <- sub("^(ga:)?([a-z0-9]+)$", "ga:\\2", tmp)
      if (str_detect(as.character(value), regex("dateofsession", ignore_case = TRUE))) {
        tmp <- "dateOfSession"
      }
      ## Replace GA Var with correct casing of valid Var Name
      ## Partial matches are accepted
      allVars <- tolower(union(kGaVars$allVars, "dateOfSession"))
      allVars_short <- union(kGaVars_df$id, "dateOfSession")
      varIndex <- charmatch(tmp, allVars)
      matches <- union(kGaVars$allVars, "dateOfSession")[varIndex]
      if (identical(any(varIndex == 0), TRUE)) {
        stop(paste0(
          "Ambiguous var name: ", paste0(value[varIndex == 0], collapse = ", "), ". Did you mean any of these?:\n",
          paste(allVars_short[str_detect(allVars_short, regex(paste0("^", tmp[varIndex == 0]), ignore_case = TRUE))], collapse = ", ")
        ))
      } else if (any(is.na(varIndex))) {
        possible_matches <- allVars_short[str_detect(allVars_short, regex(value[is.na(varIndex)], ignore_case = TRUE))]
        if (length(possible_matches) == 1) {
          matches <- possible_matches
        } else {
          stop(paste0(
            "No matching var name: ", paste0(value[is.na(varIndex)], collapse = ", "),
            if (length(possible_matches) > 0) {
              paste(
                ". Did you mean any of these?:\n",
                paste(possible_matches, collapse = ", ")
              )
            }
          ))
        }
      }
      .Object@.Data <- matches
      ## If a match was not found, use the var name supplied to
      ## let validation fail through the validObject method
      ## is.na can only work with a vector of length 1.
      if (is.na(.Object@.Data[1])) {
        .Object@.Data <- value
      }
      validObject(.Object)
    }
    return(.Object)
  }
)

setMethod(
  f = "initialize",
  signature = ".mcfVar",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      tmp <- as.character(tolower(value))
      tmp <- sub(kMcfPrefix, "mcf:", tmp)
      tmp <- sub("^(mcf:)?(.*)", "mcf:\\2", tmp)
      allVars <- with(kMcfVars, union(dims, mets))
      varIndex <- charmatch(tmp, tolower(allVars))
      tmp <- allVars[varIndex]
      .Object@.Data <- tmp
      validObject(.Object)
    }
    return(.Object)
  }
)

setMethod(
  f = "initialize",
  signature = ".rtVar",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      tmp <- as.character(tolower(value))
      tmp <- sub(kRtPrefix, "rt:", tmp)
      tmp <- sub("^(rt:)?(.*)", "rt:\\2", tmp)
      allVars <- with(kRtVars, union(dims, mets))
      varIndex <- charmatch(tmp, tolower(allVars))
      tmp <- allVars[varIndex]
      .Object@.Data <- tmp
      validObject(.Object)
    }
    .Object
  }
)

#' @describeIn Var Coerce a character to '.var'.
setMethod(
  f = "Var",
  signature = "character",
  definition = function(object) {
    as(object, ".var", strict = FALSE)
  }
)

#' @describeIn Var Set a '.var' object to a new value coerced from character.
setMethod(
  f = "Var<-",
  signature = c(".var", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

#' @describeIn Var Get the variable of an expression object.
setMethod("Var", ".expr", function(object) {object@var})

#' @describeIn Var Set the variable of an expression object using a character value to be coerced to '.var'.
setMethod(
  f = "Var<-",
  signature = c(".expr", "character"),
  definition = function(object, value) {
    object <- Expr(value, object@comparator, object@operand)
    object
  }
)

#' @describeIn Var Get the variables within a variable list object, such as sortBy, dimensions or metrics.
setMethod("Var", ".gaVarList", function(object) {object})

# Create a gaMet or gaDim object.

#' @describeIn Var GaVar takes a GA variable name and determines whether to return a Dimension or Metric object
setMethod("GaVar", "character", function(object) {as(object, ".gaVar")})

#' @describeIn Var Set the Var of a gaExpr object.
setMethod(
  f = "GaVar<-",
  signature = c(".gaVar", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

#' @describeIn Var Get the variable from expression object coerced to '.garVar'.
setMethod("GaVar", ".expr", function(object) {object@var})

#' @describeIn Var Set the variable of an expression to a .gaVar as named by a character value.
setMethod(
  f = "GaVar<-",
  signature = c(".expr", "character"),
  definition = function(object, value) {
    object@var <- value
    object
  }
)

#' @describeIn Var Get the variables of a .gaVarList.
setMethod("GaVar", ".gaVarList", function(object) {object})

# ---- McfVar ----

#' @describeIn Var McfVar takes a MCF variable and determines whether to return a Dimension or Metric object
setMethod("McfVar", "ANY", function(object) {as(object, ".mcfVar")})

# ---- RtVar ----

#' @describeIn Var McfVar takes a RT variable and determines whether to return a Dimension or Metric object
setMethod("RtVar", "ANY", function(object) {as(object, ".rtVar")})

