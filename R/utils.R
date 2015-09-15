#' @importFrom lubridate ymd
#' @importFrom stringr regex
NULL

#' IsVarMatch.
#'
#' The following method is a temporary workaround to support XX placeholders in dimension and metric
#' names, such as with custom dimensions, metrics and various goal related variables.
#'
#' @param thisVar var to compare against inVars.
#' @param inVars vector of vars to check var against.
#'
#' @keywords internal
IsVarMatch <- function(thisVar, inVars) {
  inVars <- str_replace(inVars, "XX", replacement = "[0-9]+")
  inVars <- regex(paste0("^", inVars, "$"), ignore_case = TRUE)
  any(str_detect(thisVar, inVars))
}

#' ValidGaOperand.
#'
#' Checks whether an operand value is valid for a selected dimension.
#'
#' @param var selected dimension to check operand against
#' @param operand the operand value to check
#'
ValidGaOperand <- function(var, operand) {
  test <- switch(
    var,
    "ga:date" = grepl(pattern = "^[0-9]{8}$", x = operand) &&
      (as.Date(x = operand, format = kGaDateOutFormat) >= kGaDateOrigin),
    "ga:year" = grepl(pattern = "^[0-9]{4}$", x = operand) &&
      (as.Date(x = operand, format = "%Y") >= kGaDateOrigin),
    "ga:month" = grepl(pattern = "^(0[1-9]|1[0-2])$", x = operand),
    "ga:week" = grepl(pattern = "^([0-4][1-9]|5[0-3])$", x = operand),
    "ga:day" = grepl(pattern = "^([0-2][0-9][1-9]|3[0-5][0-9]|36[0-6])$", x = operand),
    "ga:hour" = grepl(pattern = "^([01][0-9]|2[0-3])$", x = operand),
    "ga:dayOfWeek" = grepl(pattern = "^[0-6]$", x = operand),
    "ga:visitorType" = operand %in% c("New Visitor", "Returning Visitor"),
    TRUE
  )
  if (var %in% c("ga:nthMonth", "ga:nthWeek", "ga:nthDay", "ga:pageDepth", "ga:visitLength", "ga:visitCount", "ga:daysSinceLastVisit")) {
    test <- as.numeric(operand) > 0
  } else if (var %in% c("ga:searchUsed", "ga:javaEnabled", "ga:isMobile", "ga:isTablet", "ga:hasSocialSourceReferral")) {
    test <- operand %in% c("Yes", "No")
  }
  if (test) {
    return(TRUE)
  } else {
    return(paste("Invalid", var, "operand:", operand))
  }
}

#' ArgList.
#'
#' If the only argument passed was already a list, then extract that list.
#'
#' @param ... arguments or list of arguments
#'
#' @keywords internal
ArgList <- function(...) {
  as.list(
    unlist(x = list(...), recursive = FALSE)
  )
}

#' flatten.
#'
#' Flatten a nested list while preserving the class of each element
#' Convert a list type object into a non-nested list, preserving
#' the original object classes.
#'
#' @param x a list type object to flatten.
#'
#' @return a list
#'
#' @references \url{http://stackoverflow.com/a/8139959/1007029}
#'
#' @keywords internal
flatten <- function(x) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) { i <<- i + 1L; y[[i]] <<- x })
  y
}

#' CheckVectorBounds
#'
#' Check the length of each named slot within object is within the lower and
#' upper bounds specified.
#'
#' @param object an object with slots that match the names of slot_vector_bound_list
#' @param slot_vector_bound_list a named list of vectors specifying the upper
#' and lower bounds for the length of each slot of object.
#'
#' @keywords internal
CheckVectorBounds <- function(object, slot_vector_bound_list) {
  slot_vector_bounds <- data.frame(
    slot_vector_bound_list,
    row.names = c('lower', 'upper')
  )
  ret <- lapply(names(slot_vector_bounds), function(slot_name) {
    slot_length <- length(slot(object, slot_name))
    slot_bounds <- slot_vector_bounds[[slot_name]]
    names(slot_bounds) <- row.names(slot_vector_bounds)
    if (slot_length < slot_bounds['lower'] | slot_length > slot_bounds['upper']) {
      if (as.numeric(slot_bounds['lower'][1]) == as.numeric(slot_bounds['upper'][1])) {
        slot_bounds <- slot_bounds['lower']
        paste0("Slot '", slot_name, "' must be of length ", slot_bounds)
      } else {
        paste0("Slot '", slot_name, "' length must be from ",
               slot_bounds['lower'], " to ",
               slot_bounds['uppper'], "."
        )
      }
    } else {
      TRUE
    }
  })
  ret <- unlist(ret[sapply(ret, is.character)])
  if (length(ret) == 0) {
    ret <- TRUE
  }
  return(ret)
}

#' checkDataFrameClasses
#'
#' Test whether the class for each column of a data.frame match a list of
#' expected classes.
#'
#' @keywords internal
checkDataFrameClasses <- function(object, matchClasses) {
  objectClasses <- lapply(object, class)
  ret <- lapply(names(matchClasses), function(className) {
    if (!(matchClasses[className] %in% objectClasses[[className]])) {
      return(paste0("<", className, "> must be of class '", matchClasses[className], "'."))
    } else {
      TRUE
    }
  })
  invalidClasses <- sapply(ret, function(x){!identical(x, TRUE)})
  if (any(invalidClasses)) {
    return(unlist(ret[invalidClasses]))
  } else {
    TRUE
  }
}

#' split_permissions
#'
#' Take a list of character vector describing the permissions for each user and
#' transform into a nested list of users and their list of permissions.
#'
#' @keywords internal
split_permissions <- function(permissions) {
  permission_levels <- user_permission_levels
  names(permission_levels) <- permission_levels
  llply(permissions, function(permission_set) {
    y <- llply(permission_levels, function(level) {
      level %in% permission_set
    })
  })
}

#' unsplit_permissions
#'
#' Take a list of Google Analytics user permissions for a list of one or more users and
#' transform the into a list of character vector.
#'
#' @keywords internal
unsplit_permissions <- function(permissions) {
  llply(permissions, function(permission_set) {
    names(permission_set)[unlist(permission_set)]
  })
}

#' all_inherit
#'
#' Test whether all objects within a list all inherit from the same given class.
#'
#' @keywords internal
all_inherit <- function(list_object, class_names) {
  all(sapply(list_object, is, class_names))
}

#' parse_date
#'
#' Coerce a date into a character string formatted to either the
#' input or output format of the Google Analytics reporting API.
#'
#' @keywords internal
parse_date <- function(date, output_format = kGaDateInFormat) {
  format(ymd(date), format = output_format)
}

# Helper functions for coercion between classes
# ---------------------------------------------

simpleCoerce <- function(from, to) {new(to, from)}
simpleCoerceData <- function(from, to) {new(to, from@.Data)}
simpleCoerceToNumeric <- function(from, to) {new(to, as.numeric(from))}
simpleCoerceToList <- function(from, to) {new(to, list(from))}
coerceViaList <- function(from, to) {as(as.list(from), to)}
coerceViaChar <- function(from, to){as(as(from, "character"), to)}
coerceViaAnd <- function(from, to) {as(as(from, "andExpr"), to)}
simpleReplace <- function(from, value) {initialize(from, value)}
coerceLogicalOperand <- function(from, to){
  operand <- ifelse(from, yes = "Yes", no = "No")
  if (is.na(operand)) operand <- from
  new(to, operand)
}

