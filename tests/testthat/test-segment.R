library(ganalytics)

context("Segmentation queries are correctly formatted for API requests")

test_that("segment expressions are correctly coerced to character string", {
  expect_equal(
    as(
      GaSegment(
        GaSegmentFilters(
          GaCondition(GaExpr("source", "=", "google")),
          GaSequence(
            GaStartsWith(GaExpr("pagepath", "=", "/")),
            GaImmediatelyPrecedes(GaExpr("pagepath", "=", "/products/")),
            GaPrecedes(GaExpr("exitPage", "=", "/"))
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
    GaSegment(-1),
    GaSegment("gaid::-1"),
  )
  expect_identical(
    GaSegment("gaid::1"),
    GaSegment("1")
  )
  expect_equal(
    as(GaSegment("gaid::-1"), "character"),
    "gaid::-1"
  )
  expect_equal(
    as(GaSegment("gaid::1"), "character"),
    "gaid::1"
  )
})

# test_that("complex custom segment expressions are accepted by the Core Reporting API")

# condition1 <- Expr("pagepath", "=", "/")
# condition2 <- Expr("source", "=", "google")
# condition3 <- Expr("userGender", "=", "female")
#
# sequence <- Ga
