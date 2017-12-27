# library(ganalytics)
# library(lubridate)
#


# Place skip_on_cran() at the beginning of long-running tests that shouldn’t be
# run on CRAN

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

test_that("TableFilter can be used on MCF expressions", {
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

test_that("Segments<- replaces the segments of a query", {
  query <- GaQuery(view = 0)
  segment <-
    Expr("ga:source", "=", "google.com") &
      Expr("ga:deviceCategory", "=", "mobile")
  Segments(query) <- segment
  expect_identical(Segments(query)[[1]], Segments(segment)[[1]])
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

