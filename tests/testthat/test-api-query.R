context("Translation of ganalytics query into an API request.")

test_that("Queries are constructed correctly for API requests", {
  expect_equal(
    as(
      GaQuery(view = 0, startDate = "2015-01-01", endDate = "2015-01-28",
              metrics = "sessions", dimensions = "deviceCategory", sortBy = "deviceCategory",
              filters = GaExpr("source", "=", "google"), segments = GaExpr("country", "=", "Australia"),
              maxResults = 3, samplingLevel = "HIGHER_PRECISION"),
      "matrix")[,1],
    c(
      ids = "ga:0",
      `start-date` = "2015-01-01",
      `end-date` = "2015-01-28",
      metrics = "ga:sessions",
      dimensions = "ga:deviceCategory",
      sort = "ga:deviceCategory",
      filters = "ga:source==google",
      segment = "sessions::condition::ga:country==Australia",
      samplingLevel = "HIGHER_PRECISION",
      `include-empty-rows` = "false"
    )
  )
})

test_that("providing multiple view IDs, date ranges and multiple segments coerces to a multiple column matrix", {
  expect_equal(
    dim(
      as(
        GaQuery(view = c(0, 1, 2),
                startDate = c("2014-01-01", "2014-01-01"),
                endDate = c("2015-01-01", "2015-01-28"),
                metrics = "sessions", dimensions = "deviceCategory", sortBy = "deviceCategory",
                filters = GaExpr("source", "=", "google"),
                segments = GaExpr("country", "=", "Australia"),
                maxResults = 3, samplingLevel = "HIGHER_PRECISION"),
        "matrix")
    ), c(10, 6)
  )
})
