#' @include all-classes.R
NULL

# Class initialisation methods
# ----------------------------

# ---- .gaVar ----

setMethod(
  f = "initialize",
  signature = ".gaVar",
  definition = function(.Object, value) {
    if (!missing(value)) {
      tmp <- tolower(value)
      ## Substitute ga; ga- ga. ga_ with ga:
      ## Check var name starts with ga:
      ## Put ga: at start of GA var name
      tmp <- sub(kGaPrefix, "ga:", tmp)
      if (!grepl("^ga:[a-z0-9]+$", tmp)) {
        tmp <- paste("ga", tmp, sep=":")
      }
      ## Replace GA Var with correct casing of valid Var Name
      ## Partial matches are accepted
      allVars <- with(kGaVars, union(dims, mets))
      varIndex <- pmatch(tmp, tolower(allVars))
      tmp <- allVars[varIndex]
      .Object@.Data <- tmp
      ## If a match was not found, use the var name supplied
      ## As var names can only be a character vector of length 1, and
      ## as is.na will only check the first element of a vector, then
      ## only check the first element.
      if (is.na(.Object@.Data[1])) {
        .Object@.Data <- value
      }
      validObject(.Object)
#       if (is.na(tmp[1])) {
#         stop(paste("Invalid metric or dimension", value[1], sep = ": "))
#       }
    }
    return(.Object)
  }
)

# ---- .gaOperator ----

setMethod(
  f = "initialize",
  signature = ".gaOperator",
  definition = function(.Object, value) {
    if (!missing(value)) {
      if (value == "=") value <- "=="
      else if (value %in% c("!","=!")) value <- "!="
      else if (value == "<<") value <- "<"
      else if (value == ">>") value <- ">"
      else if (value == "=>") value <- ">="
      else if (value == "=<") value <- "<="
      else if (value %in% c("~=", "~")) value <- "=~"
      else if (value == "~!") value <- "!~"
      else if (value %in% c("@=", "@")) value <- "=@"
      else if (value == "@!") value <- "!@"
      .Object@.Data <- value
      validObject(.Object)
    }
    return(.Object)
  }
)


# ---- gaSegmentId ----

setMethod(
  f = "initialize",
  signature = "gaSegmentId",
  definition = function(.Object, value) {
    if (!missing(value)) {
      value <- sub(kGaPrefix, "gaid::", value)
      if (!grepl("^gaid::[0-9]+$", value)) {
        value <- paste("gaid", value, sep="::")
      }
      .Object@.Data <- value
      validObject(.Object)
    }
    return(.Object)
  }
)

setMethod(
  f = "initialize",
  signature = "gaDimExpr",
  definition = function(.Object, gaVar, gaOperator, gaOperand) {
    .Object@gaVar <- gaVar
    .Object@gaOperator <- gaOperator
    if(gaOperator %in% c("!=", "==")) {
      if(gaVar %in% c("ga:searchUsed", "ga:javaEnabled", "ga:isMobile", "ga:isTablet", "ga:hasSocialSourceReferral")) {
        yesNo <- c("Yes", "No")
        index <- pmatch(x = tolower(gaOperand), table = tolower(yesNo))
        if (is.na(index)) {
          stop(
            paste(gaVar, "Invalid operand", gaOperand, sep = ": ")
          )
        } else {
          gaOperand <- GaOperand(yesNo[index])
        }
      } else if(gaVar == "ga:visitorType") {
        visitorType <- c("New Visitor", "Returning Visitor")
        index <- pmatch(x = tolower(gaOperand), table = tolower(visitorType))
        if (is.na(index)) {
          stop(
            paste(gaVar, "Invalid operand", gaOperand, sep = ": ")
          )
        } else {
          gaOperand <- GaOperand(visitorType[index])
        }
      }
    }
    .Object@gaOperand <- gaOperand
    validObject(.Object)
    if(GaIsRegEx(.Object)) {
      GaOperand(.Object) <- tolower(GaOperand(.Object))
    }    
    return(.Object)
  }
)

# -- gaProfileId ----

setMethod(
  f = "initialize",
  signature = "gaProfileId",
  definition = function(.Object, value) {
    if (!missing(value)) {
      value <- sub(kGaPrefix, "ga:", value)
      value <- sapply(
        X = value,
        FUN = function(x) {
          if (!grepl("^ga:[0-9]+$", x)) {
            x <- paste("ga", x, sep=":")
          } else {
            x
          }
        }
      )
      .Object@.Data <- unique(value)
      validObject(.Object)
    }
    return(.Object)
  }
)

# -- gaDateRange ----

setMethod(
  f = "initialize",
  signature = "gaDateRange",
  definition = function(.Object, startDate, endDate) {
    if(!(missing(startDate)||missing(endDate))) {
      dates <- do.call(
        what = rbind,
        args = mapply(
          FUN = function(startDate, endDate) {
            data.frame(
              startDate = startDate,
              endDate = endDate,
              stringsAsFactors = FALSE
            )
          },
          startDate,
          endDate,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      )
      dates <- unique(dates)
      .Object@startDate <- dates$startDate
      .Object@endDate <- dates$endDate
      validObject(.Object)
    }
    return(.Object)
  }
)
