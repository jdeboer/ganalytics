context("Constructing Core Reporting API queries")

test_that("StartDate, EndDate, and DateRange give correct formats for API requests", {
  expect_equal(
    as(StartDate("2011-01-01"), "character"),
    "2011-01-01"
  )
  expect_equal(
    as(EndDate("2011-01-01"), "character"),
    "2011-01-01"
  )
  expect_equal(
    as(StartDate("20110101"), "character"),
    "2011-01-01"
  )
  expect_identical(
    DateRange("2011-01-01", "20110131"),
    DateRange("20110101", "2011-01-31")
  )
  expect_identical(
    DateRange("2011-01-01", "2011-01-31"),
    DateRange(as.Date("2011-01-01"), as.Date("2011-01-31"))
  )
})

test_that("Sortby parameter is corrected coerced to character", {
  expect_equal(as(
    SortBy(c("+source", "-pageviews", "+ga:hostname")),
    "character"), "ga:source,-ga:pageviews,ga:hostname")
  expect_equal(as(
    SortBy(c("-source", "+pageviews", "-ga:hostname")),
    "character"), "-ga:source,ga:pageviews,-ga:hostname")
})

test_that("Query limits are enforced", {
  expect_error(
    GaQuery(view = 0, dimensions = c(
      "source", "medium", "campaign", "adContent", "landingpage", "secondpage", "exitpage", "deviceCategory"
    )),
    "Maximum of 7 dimensions"
  )
  expect_equal(
    length(Dimensions(GaQuery(view = 0, dimensions = NULL))),
    0
  )
  # expect_error(
  #   GaQuery(view = 0, metrics = c(
  #     "users", "sessions", "pageviews", "uniquepageviews",
  #     "sessionDuration", "bounces", "transactions", "items", "transactionrevenue",
  #     "goalcompletions", "totalevents", "uniqueevents"
  #   )),
  #   "Maximum of 10 metrics"
  # )
  expect_error(
    GaQuery(view = 0, metrics = NULL)
  )
  expect_error(
    # Sort only by dimensions or metrics values that you have used in the
    # dimensions or metrics parameters. If your request sorts on a field that is
    # not indicated in either the dimensions or metrics parameter, you will
    # receive a error.
    GaQuery(view = 0, metrics = "pageviews", dimensions = "source", sortBy = "campaign")
  )
  # You can filter for a dimension or metric that is not part of your query,
  # provided all dimensions/metrics in the request and the filter are valid
  # combinations.
  expect_identical(
    TableFilter(GaExpr("pageviews", ">", 1)),
    TableFilter(
      GaQuery(view = 0, dimensions = "source", metrics = "sessions",
              filters = GaExpr("pageviews", ">", 1))
    )
  )
})
