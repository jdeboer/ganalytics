#' @include all-coercions.R
#' @include query-classes.R
#' @include all-generics.R
#' @include GaApiRequest.R
NULL

GaPaginate <- function(query, maxRequestedRows, creds, queryClass = "gaQuery") {
  # Get the first page to determine the total number of rows available.
  gaPage <- GaGetCoreReport(
    query = query,
    creds = creds,
    startIndex = 1,
    maxResults = min(maxRequestedRows, kGaMaxResults),
    queryClass = queryClass
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
  if (totalPages > 1) {
    # Step through each of the pages
    for (page in 2:totalPages) {
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
        maxResults,
        queryClass
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

GaGetCoreReport <- function(query, creds, startIndex = 1, maxResults = 10000, queryClass = "gaQuery") {
  request <- switch(
    queryClass,
    "gaQuery" = "data/ga",
    "mcfQuery" = "data/mcf",
    "rtQuery" = "data/realtime"
  )
  scope <- ga_scopes['read_only']
  query <- c(
    query,
    "start-index" = startIndex,
    "max-results" = maxResults
  )
  data.ga <- ga_api_request(creds = creds, request = request, scope = scope, queries = query)
  if (length(data.ga$error) > 1) {
    stop(with(
        data.ga$error,
        paste("Google Analytics error", code, message, sep = " : ")
    ))
  }
  data.ga <- GaListToDataframe(data.ga, queryClass = queryClass)
  return(data.ga)
}

YesNoToLogical <- function(char) {
  if (length(char) > 0) {
    char[char == "Yes"] <- "TRUE"
    char[char == "No"] <- "FALSE"
  }
  char <- as.logical(char)
  return(char)
}

# This function applies asFun to the selected columns from the data.frame, df,
# matched by colNames (case insensitive).
#
ColTypes <- function(df, colNames, asFun, ...) {
  cols <- tolower(names(df)) %in% tolower(colNames)
  if (TRUE %in% cols) df[cols] <- lapply(X = df[cols], FUN = asFun, ...)
  return(df)
}

# Some dimensions in Google Analytics, although with numerical values, should be
# treated as categorical because they dimensions rather than metrics.
FactorInt <- function(x) {
  factor(as.numeric(x), ordered = TRUE)
}

GaListToDataframe <- function(gaData, queryClass) {
  if (gaData$totalResults > 0) {
    if (queryClass == "mcfQuery") {
      gaData$rows <- llply(gaData$rows, function(row) {
        primitiveValues <- which(!is.na(row[['primitiveValue']]))
        conversionPathValues <- which(!is.null(row[['conversionPathValue']]))
        output <- list()
        output[primitiveValues] <- row[['primitiveValue']][primitiveValues]
        output[conversionPathValues] <- row[['conversionPathValue']][conversionPathValues]
        output
      })
      gaData$rows <- do.call(rbind, gaData$rows)
    }
    gaData$rows <- as.data.frame(
      gaData$rows,
      stringsAsFactors = FALSE
    )
    names(gaData$rows) <- gaData$columnHeaders$name
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$dates, asFun = as.Date, format = kGaDateOutFormat)
    gaData$rows <- ColTypes(df = gaData$rows, colNames = kGaDimTypes$orderedIntFactors, asFun = FactorInt)
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
    gaData$rows <- data.frame(cols, stringsAsFactors = FALSE)[0,]
    names(gaData$rows) <- gaData$columnHeaders$name
  }
  names(gaData$rows) <- sub("^(ga|rt|mcf)[:\\.]", "", names(gaData$rows))
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
