#Make a Goolge API request
GaApiRequest = function(baseURL, request, query, oauth) {  
  # Construct URL
  url <- paste(baseURL, "/", request, "?", query, sep="")
  url <- gsub(pattern="\\+", replacement="%2B", x=url)
  # Print the URL to the console
  message("Sending request to Google Analytics...")
  message(url)
  # Send query to Google Analytics API and capture the JSON reponse
  data.json <- oauth2.0_GET(
    url = url,
    config = sign_oauth2.0(
      access_token = oauth$getAccessToken()$access_token
    )
  )
  # Try and catch
  # Check the server response code:
  # 400 Bad Request
  message("Response received.")
  # Convert the JSON response into a R list
  data.r <- content(data.json)
  message("JSON response successfully converted into an R list.")
  # Return the list containing Google Analytics API response
  return(data.r)
}
