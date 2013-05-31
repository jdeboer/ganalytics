GaPaginate <- function(queryUrl, maxRequestedRows, oauth, quiet = FALSE, details = FALSE) {
  gaPage <- GaGetCoreReport(
    queryUrl,
    oauth,
    startIndex = 1,
    min(maxRequestedRows, kGaMaxResults),
    quiet,
    details
  )
  data <- gaPage$data
  sampled <- gaPage$sampled
  maxRows <- min(gaPage$totalResults, maxRequestedRows)
  totalPages <- ceiling(maxRows / kGaMaxResults)
  if(totalPages > 1) {
#     prog <- progress_time()
#     prog$init(totalPages)
#     prog$step()
    for(page in 2:totalPages) {
      if (!quiet) {
        message(paste0("Fetching page ", page, " of ", totalPages, "..."))
      }
      startIndex <- kGaMaxResults * (page - 1) + 1
      maxResults <- min(kGaMaxResults, (maxRows - startIndex) + 1)
      endIndex <- startIndex + maxResults - 1
      gaPage <- GaGetCoreReport(
        queryUrl,
        oauth,
        startIndex,
        maxResults,
        quiet,
        details
      )
      data <- rbind(data, gaPage$data)
#       prog$step()
    }
#     prog$term()
  }
  return(
    list(
      data = data,
      sampled = sampled
    )
  )
}
