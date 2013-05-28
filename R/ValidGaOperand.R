#' ValidGaOperand
#' 
#' Checks whether an operand value is valid for a selected dimension.
#' 
#' @param gaVar 
#' @param gaOperand 
#' 
ValidGaOperand <- function(gaVar, gaOperand) {
  if (gaVar=="ga:date") {
    if (grepl(pattern = "^[0-9]{8}$", x = gaOperand) && (as.Date(x = gaOperand, format = kGaDateOutFormat) >= kGaDateOrigin)) {
      return(TRUE)
    } else {
      return("Invalid ga:date operand")
    }
  } else if (gaVar=="ga:year") {
    if (grepl(pattern = "^[0-9]{4}$", x = gaOperand) && (as.Date(x = gaOperand, format = "%Y") >= kGaDateOrigin)) {
      return(TRUE)
    } else {
      return("Invalid ga:year operand")
    }
  } else if (gaVar == "ga:month") {
    if (grepl(pattern = "^(0[1-9]|1[0-2])$", x = gaOperand)) {
      return(TRUE)
    } else {
      return("Invalid ga:month operand")
    }
  } else if (gaVar == "ga:week") {
    if (grepl(pattern = "^([0-4][1-9]|5[0-3])$", x = gaOperand)) {
      return(TRUE)
    } else {
      return("Invalid ga:week operand")
    }
  } else if (gaVar == "ga:day") {
    if (grepl(pattern = "^([0-2][0-9][1-9]|3[0-5][0-9]|36[0-6])$", x = gaOperand)) {
      return(TRUE)
    } else {
      return("Invalid ga:day operand")
    }
  } else if (gaVar == "ga:hour") {
    if (grepl(pattern = "^([01][0-9]|2[0-3])$", x = gaOperand)) {
      return(TRUE)
    } else {
      return("Invalid ga:hour operand")
    }
  } else if (gaVar == "ga:dayOfWeek") {
    if (grepl(pattern = "^[0-6]$", x = gaOperand)) {
      return(TRUE)
    } else {
      return("Invalid ga:dayOfWeek operand")
    }  
  } else if (gaVar %in% c("ga:nthMonth", "ga:nthWeek", "ga:nthDay", "ga:pageDepth", "ga:visitLength", "ga:visitCount", "ga:daysSinceLastVisit")) {
    if (as.numeric(gaOperand) > 0) {
      return(TRUE)
    } else {
      return("Invalid operand")
    }
  } else if (gaVar %in% c("ga:searchUsed", "ga:javaEnabled", "ga:isMobile", "ga:isTablet", "ga:hasSocialSourceReferral")) {
    if (gaOperand %in% c("Yes", "No")) {
      return(TRUE)
    } else {
      return("Invalid boolean operand")
    }
  } else if (gaVar == "ga:visitorType") {
    if (gaOperand %in% c("New Visitor", "Returning Visitor")) {
      return(TRUE)
    } else {
      return("Invalid ga:visitorType operand")
    }
  } else {
    return(TRUE)
  }
}