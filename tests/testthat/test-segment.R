library(ganalytics)

context("Segmentation queries are correctly formatted for API requests")

test_that("segment expressions are correctly coerced to character string", {
  expect_equal(
    as(
      Segment(
        GaSegmentFilters(
          GaCondition(GaExpr("source", "=", "google")),
          GaSequence(
            First(GaExpr("pagepath", "=", "/")),
            Then(GaExpr("pagepath", "=", "/products/")),
            Later(GaExpr("exitPage", "=", "/"))
          ),
          scope = "sessions"
        ),
        GaSegmentFilters(
          GaCondition(GaExpr("deviceCategory", "=", "mobile")),
          scope = "users"
        )
      ),
      "character"),
    "sessions::condition::ga:source==google;sequence::^ga:pagePath==/;->ga:pagePath==/products/;->>ga:exitPagePath==/;users::condition::ga:deviceCategory==mobile")
})

test_that("segment expressions can be negated", {
  expect_equal(as(
    GaCondition(
      GaExpr("source", "=", "google"),
      negation = TRUE
    ),
    "character"), "condition::!ga:source==google")
  expect_identical(
    GaCondition(
      GaExpr("source", "=", "google"),
      negation = TRUE
    ),
    GaNot(GaCondition(
      GaExpr("source", "=", "google"),
      negation = FALSE
    ))
  )
})

test_that("segments can be selected by ID and parsed", {
  expect_identical(
    Segment(-1),
    Segment("gaid::-1"),
  )
  expect_identical(
    Segment("gaid::1"),
    Segment("1")
  )
  expect_equal(
    as(Segment("gaid::-1"), "character"),
    "gaid::-1"
  )
  expect_equal(
    as(Segment("gaid::1"), "character"),
    "gaid::1"
  )
})
