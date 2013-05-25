setMethod(
  f = "GetGaUrl",
  signature = signature(.Object = ".gaUrlClasses"),
  definition = function(.Object) {
    as(.Object, "utf8")
    # new("utf8", .Object)
  }
)

### QUERY

#Load required packages
#try and catch

# Global variables ----

#Load client secret file

#source("rGAnalyticsConfig.R")

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

# 
# GetGaData <- function(query, noSampling == FALSE) {
#   # For each segment, non-overlapping date range, and profileId...
#   #   If noSampling then
#   #     Do we expect the query will result in sampling?
#   #       If so, then split the date range
#   #   
#   #   Fetch the first page of data
#   #     If the 
#   #   If GaMaxResults(query) > kGaMaxResults then pagination is required
#   #     else 
#   #       Fetch the data
#   #     If the data is sampled, then split the date range using this formula
#   #        
#   
# #   1,000,000 maximum unique dimension combinations for any type of query. What
# #   does this mean? Suppose you request a content report for your site, which
# #   has visits to 1,000,000 unique URLs for the requested date range. In such a
# #   situation, your report would take a very long time to load in order to
# #   display all the unique URLs for that date range. To avoid this, Google
# #   Analytics retrieves a maximum of 1,000,000 unique URLs (or any other
# #   dimension value) for a given request, divided by the number of days in the
# #   request. For example: A report for the past 30 days would display
# #   approximately 30,000 unique URLs (e.g. 1,000,000/30). A report for the past
# #   60 days will display a maximum of 16,000 unique URLs (e.g. 1,000,000/60). 
# #   Because unique URLs and campaign keywords repeat across given days, this
# #   threshold is reached only by sites with lots of unique content and/or
# #   keywords.
# #   
# #   500,000 maximum sessions for special queries where the data is not already
# #   stored. In many of the reports, the list dimension is fixed, so Analytics
# #   can store this data. This enables Analytics to deliver timely reporting
# #   information for large data sets. However, if you request an ad hoc set of
# #   dimensions, that information is not stored and Analytics will need to
# #   perform the calculation at the time of the request. In this case, only
# #   500,000 sessions will be processed in order to improve the response time.
# #   Your report query might easily exceed 500,000 sessions if you request an
# #   adhoc dimension over an expanded date range. To get a sense of how many
# #   sessions might appear in your request, you can use the visits metric over
# #   the date range you intend to query. This maximum of 500,000 session applies
# #   per web property.
#   
#       # Google API error message, rCurl error, or error reported by R, etc...?


#Make a Goolge API request
GaApiRequest = function(baseURL, request, query, auth) {  
  # Construct URL
  url <- paste(baseURL, "/", request, "?", query, sep="")
  url <- gsub(pattern="\\+", replacement="%2B", x=url)
  # Print the URL to the console
  message ("Sending request to Google Analytics...")
  message (url)
  # Construct the query header containing the OAuth authentication token
  httpheader <- c(Authorization=paste("OAuth",attributes(auth)$access_token))
  # Send query to Google Analytics API and capture the JSON reponse
  # Try and catch
  # Check the server response code:
  # 400 Bad Request
  data.json <- GET(url, httpheader=httpheader)
  message ("Response received.")
  # Convert the JSON response into a R list
  tryCatch(
    data.r <- fromJSON(data.json),
    error = function(e) stop(e),
    warning = function(w) warning(w)
  )
  message ("JSON response successfully converted into an R list.")
  # Return the list containing Google Analytics API response
  return(data.r)
}
