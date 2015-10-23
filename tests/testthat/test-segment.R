library(ganalytics)

context("Segmentation queries are correctly formatted for API requests")

test_that("segment expressions are correctly coerced to character string", {
  expect_equal(
    as(
      Segment(
        SegmentFilters(
          SegmentConditionFilter(GaExpr("source", "=", "google")),
          Sequence(
            First(GaExpr("pagepath", "=", "/")),
            Then(GaExpr("pagepath", "=", "/products/")),
            Later(GaExpr("exitPage", "=", "/"))
          ),
          scope = "sessions"
        ),
        SegmentFilters(
          SegmentConditionFilter(GaExpr("deviceCategory", "=", "mobile")),
          scope = "users"
        )
      ),
      "character"),
    "sessions::condition::ga:source==google;sequence::^ga:pagePath==/;->ga:pagePath==/products/;->>ga:exitPagePath==/;users::condition::ga:deviceCategory==mobile")
})

test_that("segment expressions can be negated", {
  expect_equal(as(
    SegmentConditionFilter(
      GaExpr("source", "=", "google"),
      negation = TRUE
    ),
    "character"), "condition::!ga:source==google")
  expect_identical(
    SegmentConditionFilter(
      GaExpr("source", "=", "google"),
      negation = TRUE
    ),
    GaNot(SegmentConditionFilter(
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

test_that("PerHit returns the expected classes of output given the class of its input", {
  expect_is(
    PerHit(Expr("pageviews", "=", 1)),
    "gaSegMetExpr"
  )
  single_step_sequence <- PerHit(
    Expr("totalEvents", "=", 1) &
      Expr("pagePath", "=", "/")
  )
  expect_is(single_step_sequence, "gaSegmentSequenceFilter")
  expect_equal(length(single_step_sequence), 1)
})

test_that("Include and Exclude can be used to define segment filters", {
  expr1 <- Expr("EventCategory", "=", "video")
  expr2 <- Expr("EventAction", "=", "play")
  include_filter <- Include(expr1, expr2)
  exclude_filter <- Exclude(expr1, expr2)
  expect_is(include_filter, "gaSegmentFilterList")
  expect_is(exclude_filter, "gaSegmentFilterList")
  expect_equal(length(include_filter), 2)
  expect_equal(length(exclude_filter), 2)
  expect_true(all(sapply(include_filter, function(seg_filter) {!IsNegated(seg_filter)})))
  expect_true(all(sapply(exclude_filter, function(seg_filter) {IsNegated(seg_filter)})))
})

test_that("PerUser and PerSession can be used to scope segment filters", {
  segment_filter_list <- PerSession(Expr("pagePath", "=", "/"))
  expect_equal(as(segment_filter_list, "character"), "sessions::condition::ga:pagePath==/")
})

test_that("Non standard evaluation can be used to define conditions and sequences", {
  step1 <- Expr(~pagepath == "/")
  step2 <- Expr(~pagepath == "/cart")
  nse_sequence <- sequential_segment(list(
    step1, ..., step2
  ))
  se_sequence <- Sequence(
    step1, Later(step2)
  )
  expect_identical(nse_sequence, se_sequence)
})

