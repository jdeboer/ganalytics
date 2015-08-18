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
