#Make a Goolge API request
GaApiRequest = function(baseURL, request, query, oauth, quiet = FALSE, details = FALSE) {  
  # Construct URL
  url <- paste(baseURL, "/", request, "?", query, sep="")
  url <- gsub(pattern="\\+", replacement="%2B", x=url)
  # Print the URL to the console
  if (details) {
    message("Sending request to Google Analytics...")
    message(url)
  }
  # Send query to Google Analytics API and capture the JSON reponse
  data.json <- oauth2.0_GET(url = url, oauth = oauth)
  # Try and catch
  # Check the server response code:
  # 400 Bad Request
  # Convert the JSON response into a R list
  data.r <- content(data.json)
  data.r <- GaListToDataframe(data.r)
  # Return the list containing Google Analytics API response
  return(data.r)
}
