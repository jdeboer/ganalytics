library(ganalytics)
library(lubridate)

# check_api <- function() {
#   if (not_working()) {
#     skip("API not available")
#   }
# }
#
# test_that("foo api returns bar when given baz", {
#   check_api()
#   ...
# })

# Place skip_on_cran() at the beginning of long-running tests that shouldn’t be
# run on CRAN

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
      samplingLevel = "HIGHER_PRECISION"
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
    ), c(9, 6)
  )
})

context("MCF and RT expressions, filters, and queries are also supported")

test_that("MCF vars can be generated using McfVar", {
  expect_equal(
    as(McfVar("mcf:totalConversions"), "character"),
    "mcf:totalConversions"
  )
})

test_that("RT vars can be generated using RtVar", {
  expect_equal(
    as(RtVar("rt:activeUsers"), "character"),
    "rt:activeUsers"
  )
})

test_that("MCF and RT expressions can be created with dimensions or metrics", {
  expect_equal(
    as(McfExpr("mcf:totalConversions", ">", 10), "character"),
    "mcf:totalConversions>10"
  )
  expect_equal(
    as(RtExpr("rt:source", "=", "google"), "character"),
    "rt:source==google"
  )
})

test_that("MCF and RT queries can be constructed", {
  expect_equal(
    dim(as(McfQuery(view = 0), "matrix")),
    c(6, 1)
  )
  expect_equal(
    dim(as(RtQuery(view = 0), "matrix")),
    c(3, 1)
  )
})

context("Generalised functions can be used across all types of queries")

test_that("! can be used instead of Not", {
  expect_equal(
    !Expr("rt:activeUsers", "<=", 10),
    Expr("rt:activeUsers", ">", 10)
  )
})

test_that("TableFilter can be used instead of deprecated GaFilter function", {
  expect_is(
    TableFilter(Expr("mcf:totalConversions", ">", 10)),
    "mcfFilter"
  )
})

test_that("Expr function can used to create GA, MCF and RT expressions", {
  expect_equal(
    as(Expr("ga:source", "==", "google"), "character"),
    "ga:source==google"
  )
  expect_equal(
    as(Expr("mcf:source", "==", "google"), "character"),
    "mcf:source==google"
  )
  expect_equal(
    as(Expr("rt:source", "==", "google"), "character"),
    "rt:source==google"
  )
  expect_equal(
    as(
      Expr("mcf:source", "!=", "facebook.com") &
        (
          Expr("mcf:medium", "==", "referral") |
            Expr("mcf:campaignName", "=@", "promo")
          ),
      "character"),
    "mcf:source!=facebook.com;mcf:medium==referral,mcf:campaignName=@promo"
  )
})

test_that("Variable, Comparator and Operand compoents of an expression can be extracted" , {
  expect_equal(
    as(Operand(Expr("rt:pagePath", "=~", "^/products/")), "character"),
    "^/products/"
  )
  expect_equal(
    as(Var(Expr("mcf:source", "=~", "\\.au$")), "character"),
    "mcf:source"
  )
  expect_equal(
    as(Comparator(Expr("rt:totalEvents", ">", "10")), "character"),
    ">"
  )
})

context("New varList methods work as expected")

test_that("Only valid variable lists can be defined.", {
  expect_is(
    Metrics("ga:sessions", "ga:pageviews", "totalevents"),
    "gaMetrics"
  )
  expect_is(
    Dimensions("mcf:source", "mcf:campaignName", "mcf:medium"),
    "mcfDimensions"
  )
  expect_error(
    Metrics("ga:pageviews", "mcf:totalConversions", "rt:activeUsers")
  )
  expect_is(
    SortBy("rt:activeUsers", "rt:totalEvents", "rt:city"),
    "rtSortBy"
  )
  expect_error(
    SortBy("rt:activeUsers", "ga:totalEvents", "mcf:medium")
  )
})

context("Replace methods work as expected.")

test_that("Comparator<- replaces the comparator of an expression", {
  expr <- Expr("ga:source", "=", "google.com")
  Comparator(expr) <- "=@"
  expect_identical(expr, Expr("ga:source", "=@", "google.com"))
})

test_that("Operand<- replaces the operand of an expression", {
  expr <- Expr("ga:source", "=", "google.com")
  Operand(expr) <- "google.com.au"
  expect_identical(expr, Expr("ga:source", "==", "google.com.au"))
})

test_that("Var<- replaces the variable of an expression", {
  expr <- Expr("ga:source", "=", "google.com")
  Var(expr) <- "rt:source"
  expect_identical(expr, Expr("rt:source", "==", "google.com"))
})

test_that("TableFilter<- replaces the table filter of a query", {
  query <- GaQuery(view = 0)
  table_filter <- TableFilter(
    Expr("ga:source", "=", "google.com") &
    Expr("ga:deviceCategory", "=", "mobile")
  )
  TableFilter(query) <- table_filter
  expect_identical(TableFilter(query), table_filter)
  TableFilter(query) <- NULL
  expect_equivalent(TableFilter(query), TableFilter(NULL))
})

test_that("Segment<- replaces the segment of a query", {
  query <- GaQuery(view = 0)
  segment <-
    Expr("ga:source", "=", "google.com") &
      Expr("ga:deviceCategory", "=", "mobile")
  Segment(query) <- segment
  expect_identical(Segment(query)[[1]], Segment(segment))
})

test_that("Metrics<-, Dimensions<-, and SortBy<-, work as expected on a query", {
  query <- GaQuery(view = 0)
  Dimensions(query) <- NULL
  expect_equivalent(Dimensions(query), Dimensions(NULL))
  Dimensions(query) <- c("source", "medium")
  expect_identical(Dimensions(query), Dimensions(c("source", "medium")))
  Metrics(query) <- c("pageviews", "sessions")
  expect_equivalent(Metrics(query), Metrics(c("pageviews", "sessions")))
  SortBy(query) <- c("source", "sessions")
  expect_equivalent(SortBy(query), SortBy(c("source", "sessions")))
  SortBy(query) <- NULL
  expect_is(SortBy(query), ".sortBy")
})

context("Query variable lists - metrics, dimensions and sort-by")

test_that("Dimensions or metrics removed from a query are reflected in its sortBy", {
  query <- GaQuery(view = 0)
  Dimensions(query) <- c("eventCategory", "eventAction", "eventLabel")
  Metrics(query) <- c("totalEvents", "uniqueEvents", "eventValue")
  SortBy(query) <- c("totalEvents", "eventValue")
  expect_equivalent(SortBy(query), SortBy("totalEvents", "eventValue"))
  Metrics(query) <- "totalEvents"
  expect_equivalent(SortBy(query), SortBy("totalEvents"))
})

