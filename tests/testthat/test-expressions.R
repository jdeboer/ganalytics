context("Forming a basic condition expression")

test_that("GaExpr generates a .gaExpr object of the appropriate subclass", {
  expect_is(GaExpr("pageviews", ">", "2"), "gaMetExpr")
  expect_is(GaExpr("campaign", "=~", "(not set)"), "gaDimExpr")
  expect_is(GaExpr("pagetitle", "~", "products"), "gaDimExpr")
  expect_is(GaExpr("hostname", "!", "google.com"), "gaDimExpr")
  expect_is(GaExpr("totalevents", "=>", 5), "gaMetExpr")
  expect_error(GaExpr("entrances", "=@", "23"))
  expect_error(GaExpr("pageviews", "+", 2))
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

test_that("`Not` can be used to invert an expression", {
  expect_identical(
    Not(GaExpr("pageviews", ">", 10)),
    GaExpr("pageviews", "<=", 10)
  )
  expect_identical(
    !(GaExpr("pageviews", ">", 10)),
    GaExpr("pageviews", "<=", 10)
  )
  expect_identical(
    !Expr("eventCategory", "[]", c("video", "download")),
    Expr("eventCategory", "!~", "^(video|download)$")
  )
})

context("Combining basic condition expressions with AND and OR")

test_that("Expressions can be ORed to give the correct character output", {
  expect_equal(as(
    Or(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000)
    ),
    "character"), "ga:pageviews>100,ga:timeOnPage<2000")
  expect_equal(as(
    Or(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000),
      GaExpr("bounceRate", "=", 100)
    ),
    "character"), "ga:pageviews>100,ga:timeOnPage<2000,ga:bounceRate==100")
  expect_equal(as(
    GaExpr("pageviews", ">", 100) |
      GaExpr("timeOnPage", "<", 2000),
    "character"), "ga:pageviews>100,ga:timeOnPage<2000")
  expect_equal(as(
    GaExpr("pageviews", ">", 100) |
      (GaExpr("timeOnPage", "<", 2000) |
         GaExpr("bounceRate", "=", 100)),
    "character"), "ga:pageviews>100,ga:timeOnPage<2000,ga:bounceRate==100")
})

test_that("Expressions can be ANDed to give the correct character output", {
  expect_equal(as(
    And(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000)
    ),
    "character"), "ga:pageviews>100;ga:timeOnPage<2000")
  expect_equal(as(
    And(
      GaExpr("pageviews", ">", 100),
      GaExpr("timeOnPage", "<", 2000),
      GaExpr("bounceRate", "=", 100)
    ),
    "character"), "ga:pageviews>100;ga:timeOnPage<2000;ga:bounceRate==100")
  expect_equal(as(
    GaExpr("pageviews", ">", 100) &
      GaExpr("timeOnPage", "<", 2000),
    "character"), "ga:pageviews>100;ga:timeOnPage<2000")
  expect_equal(as(
    GaExpr("pageviews", ">", 100) &
      (GaExpr("timeOnPage", "<", 2000) &
         GaExpr("bounceRate", "=", 100)),
    "character"), "ga:pageviews>100;ga:timeOnPage<2000;ga:bounceRate==100")
})

test_that("ORed expressions can be ANDed, but ANDed expressions cannot be ORed", {
  expect_equal(as(
    And(
      Or(
        GaExpr("eventValue", "<", 50),
        GaExpr("keyword", "@", "contact")
      ),
      GaExpr("deviceCategory", "=", "mobile")
    ),
    "character"), "ga:eventValue<50,ga:keyword=@contact;ga:deviceCategory==mobile")
  expect_equal(as(
    (
      GaExpr("eventValue", "<", 50) |
        GaExpr("keyword", "@", "contact")
    ) & GaExpr("deviceCategory", "=", "mobile"),
    "character"), "ga:eventValue<50,ga:keyword=@contact;ga:deviceCategory==mobile")
  expect_error(
    Or(
      And(
        GaExpr("eventValue", "<", 50),
        GaExpr("keyword", "@", "contact")
      ),
      GaExpr("deviceCategory", "=", "mobile")
    ),
    "ANDed expressions cannot be ORed"
  )
  expect_error(
    GaExpr("eventValue", "<", 50) &
      GaExpr("keyword", "@", "contact") |
      GaExpr("deviceCategory", "=", "mobile")
  )
  expect_equal(as(
    Or(
      And(
        GaExpr("eventValue", "<", 50),
        GaExpr("keyword", "@", "contact")
      )
    ),
    "character"), "ga:eventValue<50,ga:keyword=@contact")
  expect_equal(as(
    And(
      Or(
        GaExpr("eventValue", "<", 50),
        GaExpr("keyword", "@", "contact")
      ),
      And(
        GaExpr("deviceCategory", "=", "mobile"),
        And(
          GaExpr("pageviews", ">", 100),
          GaExpr("timeOnPage", "<", 2000),
          GaExpr("bounceRate", "=", 100)
        )
      )
    ),
    "character"), "ga:eventValue<50,ga:keyword=@contact;ga:deviceCategory==mobile;ga:pageviews>100;ga:timeOnPage<2000;ga:bounceRate==100")
})

test_that("ORed expressions can be NOTed", {
  expect_identical(
    Not(Or(GaExpr("source", "=", "google"), GaExpr("medium", "=", "organic"))),
    And(GaExpr("source", "!=", "google"), GaExpr("medium", "!=", "organic"))
  )
  expect_identical(
    !(GaExpr("source", "=", "google") | GaExpr("medium", "=", "organic")),
    GaExpr("source", "!=", "google") & GaExpr("medium", "!=", "organic")
  )
})

context("Correct formatting of comparators and operands used in API queries")

test_that("expressions for each type of comparator are correctly formatted when coerced to character", {
  expect_equal(as(
    GaExpr("dateOfSession", "<>", c("2015-01-01", "2015-01-15")),
    "character"), "dateOfSession<>2015-01-01_2015-01-15")
  expect_equal(as(
    GaExpr("daysSinceLastSession", "<>", c(1, 5)),
    "character"), "ga:daysSinceLastSession<>1_5")
  expect_equal(as(
    GaExpr("pageviews", "<>", c(10, 50)),
    "character"), "ga:pageviews<>10_50")
  expect_error(GaExpr("pageviews", "<>", c(50, 10))) # Second value must be greater than first
  expect_equal(as(
    GaExpr("source", "[]", c("google", "email", "youtube")),
    "character"), "ga:source[]google|email|youtube")
  expect_error(GaExpr("source", "==", c("google", "email", "youtube")))
  expect_error(GaExpr("pageviews", "!=", c(1, 2)))
})

test_that("expressions operands are corrected depending on the type of dimension and comparator", {
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
  expect_error(Expr("istablet", "==", "maybe"), "invalid .* operand")
  expect_equal(as(
    GaExpr("usertype", "=", "returning"),
    "character"), "ga:userType==Returning Visitor")
})
