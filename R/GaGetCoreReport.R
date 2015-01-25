#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include GaApiRequest.R

GaPaginate <- function(query, maxRequestedRows, creds) {
  # Get the first page to determine the total number of rows available.
  gaPage <- GaGetCoreReport(
    query = query,
    creds = creds,
    startIndex = 1,
    maxResults = min(maxRequestedRows, kGaMaxResults)
  )
  data <- gaPage$data
  viewId <- gaPage$viewId
  # Is GA reporting sampled data?
  sampled <- gaPage$sampled
  sampleSize = gaPage$sampleSize
  sampleSpace = gaPage$sampleSpace
  # How many rows do I need in total?
  maxRows <- min(gaPage$totalResults, maxRequestedRows)
  # How many pages would that be?
  totalPages <- ceiling(maxRows / kGaMaxResults)
  if(totalPages > 1) {
    # Step through each of the pages
    for(page in 2:totalPages) {
      message(paste0("Fetching page ", page, " of ", totalPages, "..."))
      # What row am I up to?
      startIndex <- kGaMaxResults * (page - 1) + 1
      # How many rows can I request for what I need?
      maxResults <- min(kGaMaxResults, (maxRows - startIndex) + 1)
      # Get the rows of data for this page...
      gaPage <- GaGetCoreReport(
        query,
        creds,
        startIndex,
        maxResults
      )
      # append the rows to the data.frame.
      data <- rbind(data, gaPage$data)
    }
  }
  return(
    # Return the data and indicate whether there was any sampling.
    list(
      data = data,
      sampled = sampled,
      viewId = viewId,
      sampleSize = sampleSize,
      sampleSpace = sampleSpace
    )
  )
}

GaGetCoreReport <- function(query, creds, startIndex = 1, maxResults = 10000) {
  request <- "data/ga"
  scope <- "https://www.googleapis.com/auth/analytics.readonly"
  query <- c(
    query,
    "start-index" = startIndex,
    "max-results" = maxResults
  )
  data.ga <- ga_api_request(creds = creds, request = request, scope = scope, queries = query)
  if (!is.null(data.ga$error)) {
    stop(with(
        data.ga$error,
        paste("Google Analytics error", code, message, sep = " : ")
    ))
  }
  data.ga <- GaListToDataframe(data.ga)
  return(data.ga)
}

YesNoToLogical <- function(char) {
  if (length(char) > 0) {
    char[char=="Yes"] <- "TRUE"
    char[char=="No"] <- "FALSE"
  }
  char <- as.logical(char)
  return(char)
}

ColTypes <- function(df, colNames, asFun, ...) {
  #cols <- aaply(tolower(names(df)), 1, function(df_col) {any(str_detect(df_col, colNames))})
  cols <- tolower(names(df)) %in% tolower(colNames)
  if(TRUE %in% cols) df[cols] <- lapply(X = df[cols], FUN = asFun, ...)
  return(df)
}

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
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$bools, asFun = YesNoToLogical)
    metric_cols <- gaData$columnHeaders$name[gaData$columnHeaders$columnType == "METRIC"]
    gaData$rows[metric_cols] <- data.frame(llply(gaData$rows[metric_cols], as.numeric))
    #gaData$rows <- as.numeric(gaData$rows[metric_cols])
    #gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaVars$mets, asFun = as.numeric)
    charCols <- lapply(gaData$rows, class) == "character"
    gaData$rows <- ColTypes(df = gaData$rows, colNames = names(charCols)[charCols], asFun = factor)
  } else {
    cols <- as.list(gaData$columnHeaders$name)
    names(cols) <- cols
    gaData$rows <- data.frame(cols)[0,]
    names(gaData$rows) <- gaData$columnHeaders$name
  }
  names(gaData$rows) <- sub("^ga[:\\.]", "", names(gaData$rows))
  return(
    list(
      data = gaData$rows,
      totalResults = max(gaData$totalResults, 1),
      sampled = gaData$containsSampledData,
      sampleSize = gaData$sampleSize,
      sampleSpace = gaData$sampleSpace,
      viewId = gaData$query$ids
    )
  )
}
