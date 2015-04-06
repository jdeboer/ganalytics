library(ganalytics)

context("Selecting a dimension and metric variable")

test_that("GaVar generates a .gaVar object of the appropriate subclass", {
  expect_is(GaVar("ga:source"), "gaDimVar")
  expect_is(GaVar("ga:pageviews"), "gaMetVar")
  expect_is(GaVar("dateOfSession"), "gaDimVar")
  expect_error(GaVar("NotAVariable"))
})

test_that("Initialisation of a .gaVar object corrects the variable name", {
  expect_equal(GaVar("medium"), GaVar("ga:medium"))
  expect_equal(GaVar("pagepath"), GaVar("ga:pagePath"))
  expect_equal(GaVar("landingpage"), GaVar("ga:landingPagePath"))
  expect_equal(GaVar("dateofsess"), GaVar("dateOfSession"))
  expect_equal(GaVar("completionsall"), GaVar("ga:goalCompletionsAll"))
})

context("Forming a basic condition expression")

test_that("GaExpr generates a .gaExpr object of the appropriate subclass", {
  expect_is(GaExpr("pageviews", ">", "2"), "gaMetExpr")
  expect_is(GaExpr("campaign", "=~", "(not set)"), "gaDimExpr")
  expect_is(GaExpr("pagetitle", "~", "products"), "gaDimExpr")
  expect_is(GaExpr("hostname", "!", "google.com"), "gaDimExpr")
  expect_is(GaExpr("totalevents", "=>", 5), "gaMetExpr")
  expect_error(GaExpr("entrances", "=@", "23"))
})

test_that(".gaExpr objects coerce to character", {
  expect_equal(as(GaExpr("eventcategory", "~=", "^video"), "character"), "ga:eventCategory=~^video")
  expect_equal(as(GaExpr("ga:uniquePageviews", ">", 5), "character"), "ga:uniquePageviews>5")
  expect_equal(as(GaExpr("pagePath", "~", "[;,\\-].+"), "character"), "ga:pagePath=~[\\;\\,\\\\-].+")
})

test_that("GaNot can be used to invert an expression", {
  expect_equal(GaNot(GaExpr("pageviews", ">", 10)), GaExpr("pageviews", "<=", 10))
})

context("Combining basic condition expressions with AND and OR")

test_that("Expressions can be ORed to give the correct character output", {
  expect_equal(as(
    GaOr(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000)
    ),
    "character"), "ga:pageviews>100,ga:timeOnPage<2000")
  expect_equal(as(
    GaOr(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000),
      GaExpr("bounceRate", "=", 100)
    ),
    "character"), "ga:pageviews>100,ga:timeOnPage<2000,ga:bounceRate==100")
})

test_that("Expressions can be ANDed to give the correct character output", {
  expect_equal(as(
    GaAnd(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000)
    ),
    "character"), "ga:pageviews>100;ga:timeOnPage<2000")
  expect_equal(as(
    GaAnd(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000),
      GaExpr("bounceRate", "=", 100)
    ),
    "character"), "ga:pageviews>100;ga:timeOnPage<2000;ga:bounceRate==100")
})

test_that("ORed expressions can be ANDed, but ANDed expressions cannot be ORed", {
  expect_equal(as(
    GaAnd(
      GaOr(
        GaExpr("eventValue", "<", 50),
        GaExpr("keyword", "@", "contact")
      ),
      GaExpr("deviceCategory", "=", "mobile")
    ),
    "character"), "ga:eventValue<50,ga:keyword=@contact;ga:deviceCategory==mobile")
#   expect_error(
#     GaOr(
#       GaAnd(
#         GaExpr("eventValue", "<", 50),
#         GaExpr("keyword", "@", "contact")
#       ),
#       GaExpr("deviceCategory", "=", "mobile")
#     )
#   )
})

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
    GaFilter(GaOr(GaExpr("pageviews", ">", 10), GaExpr("entrances", "<", 5))),
    "character"), "ga:pageviews>10,ga:entrances<5")
  expect_error(
    GaFilter(GaOr(GaExpr("landingPagePath", "=", "/"), GaExpr("entrances", "<", 5)))
  )
  expect_equal(as(
    GaFilter(GaAnd(GaExpr("landingPagePath", "=", "/"), GaExpr("entrances", "<", 5))),
    "character"), "ga:landingPagePath==/;ga:entrances<5")
  expect_error(
    GaFilter(
      GaAnd(
        GaOr(
          GaExpr("eventValue", "<", 50),
          GaExpr("keyword", "@", "contact")
        ),
        GaExpr("deviceCategory", "=", "mobile")
      )
    )
  )
})

