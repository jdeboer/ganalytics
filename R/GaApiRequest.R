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
