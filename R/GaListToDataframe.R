
#' @include ColTypes.R
#' @include YesNo2Logical.R
NULL

FactorInt <- function(x) {
  factor(as.numeric(x), ordered = TRUE)
}

GaListToDataframe <- function(gaData) {
  if (gaData$totalResults > 0) {
    gaData$rows <- as.data.frame(
      gaData$rows,
      stringsAsFactors = FALSE
    )
    names(gaData$rows) <- gaData$columnHeaders$name
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$dates, asFun = as.Date, format = kGaDateOutFormat)
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$orderedFactors, asFun = FactorInt)
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$nums, asFun = as.numeric)
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$bools, asFun = YesNo2Logical)
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaVars$mets, asFun = as.numeric)
    charCols <- lapply(gaData$rows, class) == "character"
    gaData$rows <- ColTypes(df = gaData$rows, colNames = names(charCols)[charCols], asFun = factor)
  } else {
    cols <- as.list(names(gaData$rows) <- gaData$columnHeaders$name)
    names(cols) <- cols
    gaData$rows <- data.frame(cols)[0,]
  }
  names(gaData$rows) <- sub("^ga:", "", names(gaData$rows))
  return(
    list(
      data = gaData$rows,
      totalResults = max(gaData$totalResults, 1),
      sampled = gaData$containsSampledData
    )
  )
}