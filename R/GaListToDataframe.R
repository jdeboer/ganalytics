#' @include ColTypes.R
#' @include YesNo2Logical.R
NULL

GaListToDataframe <- function(gaData) {
  gaData$columnHeaders <- as.data.frame(do.call(rbind, gaData$columnHeaders))
  if (gaData$totalResults > 0) {
    gaData$rows <- as.data.frame(
      x = do.call(rbind, gaData$rows),
      stringsAsFactors = FALSE
    )
  } else {
    cols <- as.list(as.character(gaData$columnHeaders$name))
    names(cols) <- cols
    gaData$rows <- data.frame(cols)[0,]
  }
  names(gaData$rows) <- gaData$columnHeaders$name
  gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$dates, asFun = as.Date, format = kGaDateOutFormat)
  gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$ints, asFun = as.integer)
  gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$nums, asFun = as.numeric)
  gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$bools, asFun = YesNo2Logical)
  gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaVars$mets, asFun = as.numeric)
  names(gaData$rows) <- sub("^ga:", "", names(gaData$rows))
  return(
    list(
      data = gaData$rows,
      totalResults = max(gaData$totalResults, 1),
      sampled = gaData$containsSampledData
    )
  )
}