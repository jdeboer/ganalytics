#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

setMethod(
  f = "GetGaUrl",
  signature = signature(".gaUrlClasses"),
  definition = function(.Object) {
    message("hello")
    as(.Object, "utf8")
  }
)

### QUERY

ColTypes <- function(df, colNames, asFun, ...) {
  cols <- tolower(names(df)) %in% tolower(colNames)
  if(TRUE %in% cols) df[cols] <- lapply(X = df[cols], FUN = asFun, ...)
  return(df)
}

YesNo2Logical <- function(char) {
  if (length(char) > 0) {
    char[char=="Yes"] <- "TRUE"
    char[char=="No"] <- "FALSE"
  }
  char <- as.logical(char)
  return(char)
}

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

GaGetCoreReport <- function(queryUrl, authFile, startIndex = 1, maxResults = 10000) {
  request <- ""
  queryUrl <- paste(
    queryUrl,
    paste(
      paste("start-index", startIndex, sep = "="),
      paste("max-results", maxResults, sep = "="),
      sep = "&"
    ),
    sep = "&"
  )
  data.ga <- GaApiRequest(reporting.api, request, queryUrl, authFile)
if (!is.null(data.ga$error)) {
  stop(
    with(
      data = data.ga$error,
      expr = paste("Google Analytics error", code, message, sep=" : ")
    )
  )
}
return(data.ga)
}

GetGaData <- function(query) {
  authFile <- query@authFile
  # try and catch  
  print(paste("Token auth file provided: ", authFile))  
  if (authFile=="") {
    message("Authentication token file not provided!")
    stop("Error: Please specify the auth token file's location.")
  }
  maxRequestedRows <- GaMaxResults(query)
  queryURLs <- GetGaUrl(query)
  data.ga <- NULL
  sampled <- FALSE
  for (queryUrl in queryURLs) {
    gaResults <- GaListToDataframe(
      GaGetCoreReport(queryUrl, authFile, startIndex = 1, min(maxRequestedRows, kGaMaxResults))                
    )
    if(gaResults$sampled) sampled <- TRUE
    data.ga <- rbind(
      data.ga,
      gaResults$data
    )
    maxRows <- min(gaResults$totalResults, maxRequestedRows)
    if(maxRows > 1) {
      totalPages <- ceiling(maxRows / kGaMaxResults)
      if(totalPages > 1) {
        for(page in 2:totalPages) {
          startIndex <- kGaMaxResults * (page - 1) + 1
          maxResults <- min(kGaMaxResults, (maxRows - startIndex) + 1)
          gaResults <- GaListToDataframe(
            GaGetCoreReport(queryUrl, authFile, startIndex, maxResults)                
          )
          data.ga <- rbind(
            data.ga,
            gaResults$data
          )
        }
      }
    }
  }
  charCols <- lapply(data.ga, class) == "character"
  data.ga <- ColTypes(df = data.ga, colNames = names(charCols)[charCols], asFun = as.factor)
  if (sampled) warning("Contains sampled data.")
  return(data.ga)
}

#Make a Goolge API request
GaApiRequest = function(baseURL, request, query, auth) {  
  # Construct URL
  url <- paste(baseURL, "/", request, "?", query, sep="")
  url <- gsub(pattern="\\+", replacement="%2B", x=url)
  # Print the URL to the console
  message ("Sending request to Google Analytics...")
  message (url)
  # Construct the query header containing the OAuth authentication token
  httpheader <- c(Authorization = paste("OAuth", attributes(auth)$access_token))
  # Send query to Google Analytics API and capture the JSON reponse
  # Try and catch
  # Check the server response code:
  # 400 Bad Request
  data.json <- GET(url, add_headers(httpheader))
  message ("Response received.")
  # Convert the JSON response into a R list
  tryCatch(
    data.r <- content(data.json),
    error = function(e) stop(e),
    warning = function(w) warning(w)
  )
  message ("JSON response successfully converted into an R list.")
  # Return the list containing Google Analytics API response
  return(data.r)
}
