check_api <- function() {
  if (length(get_creds()) == 0) {
    skip("API not available because no API credentials were found.")
  } else {
    tryCatch(
      view <- GaAccount("30481")$properties[["UA-30481-1"]]$views[["1174"]],
      error = function(e) {
        skip("View used for testing is unavailable.")
      }
    )
  }
}

context("Parsing Google Analytics Core Reporting API responses")

test_that("httr response object can be parsed into a nested list", {
  ga_response <- readRDS(
    system.file("extdata", "ga-query-response1.RDS", package = "ganalytics")
  )
  parsed_response <- response_to_list(ga_response)
  all(c(
    "query",
    "totalResults",
    "profileInfo",
    "containsSampledData",
    "columnHeaders",
    "totalsForAllResults",
    "rows"
  ) %in% names(parsed_response))
  expect_identical(
    parsed_response$query,
    list(
      `start-date` = "2015-11-30",
      `end-date` = "2015-12-07",
      ids = "ga:1174",
      dimensions = "ga:date",
      metrics = "ga:sessions",
      `start-index` = 1L,
      `max-results` = 10000L,
      samplingLevel = "DEFAULT"
    )
  )
  expect_identical(
    parsed_response$profileInfo[c(
      "profileId",
      "accountId",
      "webPropertyId"
    )],
    list(
      profileId = "1174",
      accountId = "30481",
      webPropertyId = "UA-30481-1"
    )
  )

})

test_that("A basic query can be executed against the Google Store GA account", {
  check_api()
  q <- GaQuery(view = "1174")
  results <- GetGaData(q)
  expect_is(results, "data.frame")
  expect_equivalent(dim(results), c(8, 2))
  expect_named(results, c("date", "sessions"))
  expect_equivalent(sapply(results, class), c("Date", "numeric"))
})
