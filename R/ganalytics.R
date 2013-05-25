#' Google Analytics client for R
#' 
### CONFIG

# Set global variables

#Google APIs OAuth2.0 URLs
auth_url <- "https://accounts.google.com/o/oauth2/auth"
token_url <- "https://accounts.google.com/o/oauth2/token"
scope_url <- "https://www.googleapis.com/auth/analytics.readonly"

#Google APIs requests base URLs
management.api <- "https://www.googleapis.com/analytics/v3/management"
reporting.api <- "https://www.googleapis.com/analytics/v3/data/ga"

kGaDateOutFormat <- "%Y%m%d"
# The earliest valid date is 20050101. There is no upper limit restriction for a start-date.
kGaDateOrigin = as.Date("2005-01-01")
# kGaDateOutRegEx <- "^[2-9](00[5-9]|0[1-9][0-9]|[1-9][0-9]{2})(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])$"

# Global data: valid GA variables and operators ----

kGaDimTypes <- list(
  dates = "ga:date",
  ints = c(
    "ga:dayOfWeek",
    "ga:hour",
    "ga:year",
    "ga:month",
    "ga:week",
    "ga:day",
    "ga:visitLength",
    "ga:daysSinceLastVisit",
    "ga:visitCount",
    "ga:nthDay",
    "ga:nthWeek",
    "ga:nthMonth"
  ),
  nums = c(
    "ga:latitude",
    "ga:longitude"
  ),
  bools = c(
    "ga:isMobile",
    "ga:isTablet",
    "ga:javaEnabled",
    "ga:searchUsed"
  )
)

#print(getwd())
#data("kGaVars",  package = character(0), envir = environment())

kGaOps <- list(
  met = c("==","!=","<",">","<=",">="),
  dim = c("==","!=","=~","!~","=@","!@")
)

kGaPrefix <- "^ga[;\\-\\._]"

kGaMax <- list(
  dimensions = 7,
  metrics = 10
)

kGaDateInFormat <- "%Y-%m-%d"

## If the only argument passed was already a list, then extract that list.
ArgList <- function(...) {
  exprList <- as.list(
    unlist(x = list(...), recursive = FALSE)
  )
}

kGaMaxResults <- 10000L
kGaMaxRows <- 1000000L


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
