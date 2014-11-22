GaPaginate <- function(query, maxRequestedRows, creds, quiet = FALSE, details = FALSE) {
  # Get the first page to determine the total number of rows available.
  gaPage <- GaGetCoreReport(
    query = query,
    creds = creds,
    startIndex = 1,
    maxResults = min(maxRequestedRows, kGaMaxResults),
    quiet = quiet,
    details = details
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
      if (!quiet) {
        message(paste0("Fetching page ", page, " of ", totalPages, "..."))
      }
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
        quiet,
        details
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
