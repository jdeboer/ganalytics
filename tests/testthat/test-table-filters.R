library(ganalytics)

context("Using expressions to create filters")

test_that("a filter can be a basic expression", {
  expect_is(
    GaFilter(GaExpr("dimension1", "!", "yellow")),
    "gaFilter"
  )
  expect_equal(
    as(GaFilter(GaExpr("dimension1", "!", "yellow")), "character"),
    "ga:dimension1!=yellow"
  )
})

test_that("ANDed (but not ORed) filter expressions may mix dimensions with metrics", {
  expect_equal(as(
    GaFilter(Or(GaExpr("pageviews", ">", 10), GaExpr("entrances", "<", 5))),
    "character"), "ga:pageviews>10,ga:entrances<5")
  expect_error(
    GaFilter(Or(GaExpr("landingPagePath", "=", "/"), GaExpr("entrances", "<", 5))),
    "cannot mix metrics and dimensions"
  )
  expect_equal(as(
    GaFilter(And(GaExpr("landingPagePath", "=", "/"), GaExpr("entrances", "<", 5))),
    "character"), "ga:landingPagePath==/;ga:entrances<5")
  expect_error(
    GaFilter(
      And(
        Or(
          GaExpr("eventValue", "<", 50),
          GaExpr("keyword", "@", "contact")
        ),
        GaExpr("deviceCategory", "=", "mobile")
      )
    )
  )
})

test_that("Filter expressions cannot use '[]' or '<>' comparators", {
  expect_error(
    GaFilter(GaExpr("medium", "[]", c("organic", "cpc"))),
    "\\[\\]"
  )
  expect_error(
    GaFilter(GaExpr("pageviews", "<>", c(10, 100))),
    "<>"
  )
})
