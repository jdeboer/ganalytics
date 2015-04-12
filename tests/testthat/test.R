library(ganalytics)

context("Selecting a dimension and metric variable")

test_that("GaVar generates a .gaVar object of the appropriate subclass", {
  expect_is(GaVar("ga:source"), "gaDimVar")
  expect_is(GaVar("ga:pageviews"), "gaMetVar")
  expect_is(GaVar("dateOfSession"), "gaDimVar")
  expect_error(GaVar("NotAVariable"))
})

test_that("Initialisation of a .gaVar object corrects the variable name", {
  expect_identical(GaVar("medium"), GaVar("ga:medium"))
  expect_identical(GaVar("pagepath"), GaVar("ga:pagePath"))
  expect_identical(GaVar("landingpage"), GaVar("ga:landingPagePath"))
  expect_identical(GaVar("dateofsess"), GaVar("dateOfSession"))
  expect_identical(GaVar("completionsall"), GaVar("ga:goalCompletionsAll"))
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
  expect_equal(as(
    GaExpr("eventcategory", "~=", "^video"),
    "character"), "ga:eventCategory=~^video")
  expect_equal(as(
    GaExpr("ga:uniquePageviews", ">", 5),
    "character"), "ga:uniquePageviews>5")
  expect_equal(as(
    GaExpr("pagePath", "~", "[;,\\-].+"),
    "character"), "ga:pagePath=~[\\;\\,\\\\-].+")
})

test_that("GaNot can be used to invert an expression", {
  expect_identical(
    GaNot(GaExpr("pageviews", ">", 10)),
    GaExpr("pageviews", "<=", 10)
  )
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
  expect_error(
    GaOr(
      GaAnd(
        GaExpr("eventValue", "<", 50),
        GaExpr("keyword", "@", "contact")
      ),
      GaExpr("deviceCategory", "=", "mobile")
    )
  )
})

test_that("ORed expressions can be NOTed", {
  expect_identical(
    GaNot(GaOr(GaExpr("source", "=", "google"), GaExpr("medium", "=", "organic"))),
    GaAnd(GaExpr("source", "!=", "google"), GaExpr("medium", "!=", "organic"))
  )
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

context("Correct formatting of operators and operands used in API queries")

test_that("expressions for each type of operator are correctly formatted when coerced to character", {
  expect_equal(as(
    GaExpr("dateOfSession", "<>", c("2015-01-01", "2015-01-15")),
    "character"), "dateOfSession<>2015-01-01_2015-01-15")
  expect_equal(as(
    GaExpr("source", "[]", c("google", "email", "youtube")),
    "character"), "ga:source[]google|email|youtube")
})

test_that("expressions operands are corrected depending on the type of dimension and operator", {
  expect_equal(as(
    GaExpr("date", "!=", "2014-01-01"),
    "character"), "ga:date!=20140101")
  expect_equal(as(
    GaExpr("date", "!=", "20140101"),
    "character"), "ga:date!=20140101")
  expect_equal(as(
    GaExpr("istablet", "=", TRUE),
    "character"), "ga:isTablet==Yes")
  expect_equal(as(
    GaExpr("istablet", "=", "no"),
    "character"), "ga:isTablet==No")
  expect_equal(as(
    GaExpr("usertype", "=", "returning"),
    "character"), "ga:userType==Returning Visitor")
})

context("Segmentation queries are correctly format for API requests")

test_that("segment expressions are correctly coerced to character string", {
  expect_equal(
    as(
      GaSegment(
        GaSegmentCondition(
          GaNonSequenceCondition(GaExpr("source", "=", "google")),
          GaSequenceCondition(
            GaStartsWith(GaExpr("pagepath", "=", "/")),
            GaImmediatelyPrecedes(GaExpr("pagepath", "=", "/products/")),
            GaPrecedes(GaExpr("exitPage", "=", "/"))
          ),
          scope = "sessions"
        ),
        GaSegmentCondition(
          GaNonSequenceCondition(GaExpr("deviceCategory", "=", "mobile")),
          scope = "users"
        )
      ),
      "character"),
    "sessions::condition::ga:source==google;sequence::^ga:pagePath==/;->ga:pagePath==/products/;->>ga:exitPagePath==/;users::condition::ga:deviceCategory==mobile")
})

test_that("segment expressions can be negated", {
  expect_equal(as(
    GaNonSequenceCondition(
      GaExpr("source", "=", "google"),
      negation = TRUE
    ),
    "character"), "condition::!ga:source==google")
  expect_identical(
    GaNonSequenceCondition(
      GaExpr("source", "=", "google"),
      negation = TRUE
    ),
    GaNot(GaNonSequenceCondition(
      GaExpr("source", "=", "google"),
      negation = FALSE
    ))
  )
})

context("Constructing Core Reporting API queries")

test_that("Queries are constructed correctly for API requests", {
  expect_equal(
    as(
      GaQuery(view = 0, startDate = "2015-01-01", endDate = "2015-01-28",
              metrics = "sessions", dimensions = "deviceCategory", sortBy = "deviceCategory",
              filters = GaExpr("source", "=", "google"), segment = GaExpr("country", "=", "Australia"),
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
                segment = GaExpr("country", "=", "Australia"),
                maxResults = 3, samplingLevel = "HIGHER_PRECISION"),
        "matrix")
    ), c(9, 6)
  )
})