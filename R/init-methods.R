#' @include all-classes.R
#' @importFrom stringr str_match
#' @importFrom lubridate ymd
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
      tmp <- sub("^(ga:)?([a-z0-9]+)$", "ga:\\2", tmp)
      if(str_detect(value, ignore.case("dateofsession"))) {
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
          paste(allVars_short[str_detect(allVars_short, ignore.case(paste0("^", tmp[varIndex == 0])))], collapse = ", ")
        ))
      } else if (any(is.na(varIndex))) {
        possible_matches <- allVars_short[str_detect(allVars_short, ignore.case(value[is.na(varIndex)]))]
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
  definition = function(.Object, value) {
    if (!missing(value)) {
      tmp <- tolower(value)
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
  definition = function(.Object, value) {
    if (!missing(value)) {
      tmp <- tolower(value)
      tmp <- sub(kRtPrefix, "rt:", tmp)
      tmp <- sub("^(rt:)?(.*)", "rt:\\2", tmp)
      allVars <- with(kRtVars, union(dims, mets))
      varIndex <- charmatch(tmp, tolower(allVars))
      tmp <- allVars[varIndex]
      .Object@.Data <- tmp
      validObject(.Object)
    }
    return(.Object)
  }
)

# ---- .operator ----

setMethod(
  f = "initialize",
  signature = ".operator",
  definition = function(.Object, value) {
    if (!missing(value)) {
      if (value == "=") value <- "=="
      else if (value %in% c("!","=!")) value <- "!="
      else if (value == "][") value <- "[]"
      else if (value == "><") value <- "<>"
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
    .Object
  }
)

# ---- gaSegmentId ----

setMethod(
  f = "initialize",
  signature = "gaSegmentId",
  definition = function(.Object, value) {
    if (!missing(value)) {
      value <- sub(kGaPrefix, "gaid::", value)
      if (!grepl("^gaid::\\-?[0-9]+$", value)) {
        value <- paste("gaid", value, sep = "::")
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
  definition = function(.Object, var, operator, operand) {
    .Object@var <- var
    .Object@operator <- operator
    if(operator %in% c("!=", "==", "[]", "<>")) {
      if(var %in% kGaDimTypes$bools) {
        yesNo <- c("No" = FALSE, "Yes" = TRUE)
        index <- pmatch(x = tolower(operand), table = tolower(names(yesNo)))
        if (is.na(index)) {
          index <- which(operand == yesNo)
          if (length(index) == 1) {
            operand <- Operand(names(yesNo)[index])
          } else {
            stop(paste(var, "Invalid operand", operand, sep = ": "))
          }
        } else {
          operand <- Operand(names(yesNo)[index])
        }
      } else if(var %in% c("ga:visitorType", "ga:userType")) {
        visitorType <- c("New Visitor", "Returning Visitor")
        index <- pmatch(x = tolower(operand), table = tolower(visitorType))
        if (is.na(index)) {
          stop(paste(var, "Invalid operand", operand, sep = ": "))
        } else {
          operand <- Operand(visitorType[index])
        }
      } else if(var == "ga:date") {
        operand <- Operand(format(ymd(operand), format = "%Y%m%d"))
      } else if(var == "dateOfSession") {
        operand <- Operand(format(ymd(operand), format = "%Y-%m-%d"))
      }
    }
    .Object@operand <- operand
    if(IsRegEx(.Object)) {
      Operand(.Object) <- tolower(Operand(.Object))
    }
    validObject(.Object)
    .Object
  }
)

# -- gaProfileId ----

setMethod(
  f = "initialize",
  signature = "gaProfileId",
  definition = function(.Object, value) {
    if (!missing(value)) {
      value <- sub(kGaPrefix, "ga:", value)
      value <- sapply(value, function(x) {
        if (!grepl("^ga:[0-9]+$", x)) {
          x <- paste("ga", x, sep = ":")
        }
        x
      })
      .Object@.Data <- unique(value)
      validObject(.Object)
    }
    .Object
  }
)

# -- gaDateRange ----

setMethod(
  f = "initialize",
  signature = "gaDateRange",
  definition = function(.Object, startDate, endDate) {
    # If startDate and endDate are provided then
    # bind every combination of startDate and endDate
    # into a data.frame, keep only the unique rows,
    # and use these start and end dates for this object.
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
    .Object
  }
)
