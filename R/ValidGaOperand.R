#' ValidGaOperand
#' 
#' Checks whether an operand value is valid for a selected dimension.
#' 
#' @param gaVar selected dimension to check operand against
#' @param gaOperand the operand value to check
#' 
ValidGaOperand <- function(gaVar, gaOperand) {
  test <- switch(
    gaVar,
    "ga:date" = grepl(pattern = "^[0-9]{8}$", x = gaOperand) &&
      (as.Date(x = gaOperand, format = kGaDateOutFormat) >= kGaDateOrigin),
    "ga:year" = grepl(pattern = "^[0-9]{4}$", x = gaOperand) &&
      (as.Date(x = gaOperand, format = "%Y") >= kGaDateOrigin),
    "ga:month" = grepl(pattern = "^(0[1-9]|1[0-2])$", x = gaOperand),
    "ga:week" = grepl(pattern = "^([0-4][1-9]|5[0-3])$", x = gaOperand),
    "ga:day" = grepl(pattern = "^([0-2][0-9][1-9]|3[0-5][0-9]|36[0-6])$", x = gaOperand),
    "ga:hour" = grepl(pattern = "^([01][0-9]|2[0-3])$", x = gaOperand),
    "ga:dayOfWeek" = grepl(pattern = "^[0-6]$", x = gaOperand),
    "ga:visitorType" = gaOperand %in% c("New Visitor", "Returning Visitor"),
    TRUE
  )
  if (gaVar %in% c("ga:nthMonth", "ga:nthWeek", "ga:nthDay", "ga:pageDepth", "ga:visitLength", "ga:visitCount", "ga:daysSinceLastVisit")) {
    test <- as.numeric(gaOperand) > 0
  } else if (gaVar %in% c("ga:searchUsed", "ga:javaEnabled", "ga:isMobile", "ga:isTablet", "ga:hasSocialSourceReferral")) {
    test <- gaOperand %in% c("Yes", "No")
  }
  if (test) {
    return(TRUE)
  } else {
    return(paste("Invalid", gaVar, "operand:", gaOperand))
  }
}
