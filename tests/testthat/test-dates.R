library(ganalytics)
library(lubridate)

context("Date functions")

test_that("functions DateRange, StartDate, EndDate, and their replacement versions work across all required signatures", {
  # character dateRange date query
  expect_equivalent(StartDate("2010-01-31"), as.Date("2010-01-31"))
  expect_equivalent(EndDate("20100131"), as.Date("2010-01-31"))
  date_range <- DateRange("2010-01-01", "2010-01-31")
  expect_equivalent(DateRange(date_range), date_range)
  expect_equivalent(StartDate(date_range), as.Date("2010-01-01"))
  expect_equivalent(EndDate(date_range), as.Date("2010-01-31"))
  EndDate(date_range) <- "2011-01-31"
  expect_equivalent(EndDate(date_range), as.Date("2011-01-31"))
  expect_error(StartDate(date_range) <- "20110201", "cannot be before")
  StartDate(date_range) <- as.Date("2011-01-01")
  expect_equivalent(StartDate(date_range), as.Date("2011-01-01"))
  query <- GaQuery(view = 0)
  DateRange(query) <- date_range
  expect_equivalent(DateRange(query), date_range)
  date_range <- as.Date(c("2010-01-01", "2010-01-31"))
  DateRange(query) <- date_range
  expect_equivalent(DateRange(query), DateRange("2010-01-01", "2010-01-31"))
  #date_range1 <- DateRange(as.Date("2012-01-01"), as.Date("2012-01-31"))
  #date_range2 <- DateRange("2012-01-01", as.Date("2012-01-31"))
  #date_range3 <- DateRange(as.Date("2012-01-01"), "2012-01-31")
})

test_that("lubridate date Interval objects can be used with DateRange", {
  date_range <- interval(as.Date("2010-01-01"), as.Date("2010-01-31"))
  expect_equal(DateRange(date_range), DateRange("2010-01-01", "2010-01-31"))
  date_range <- interval(as.Date("2010-01-31"), as.Date("2010-01-01"))
  expect_equal(DateRange(date_range), DateRange("2010-01-01", "2010-01-31"))
  query <- GaQuery(view = 0)
  DateRange(query) <- date_range
  expect_equal(StartDate(query), StartDate(date_range))
  expect_equal(EndDate(query), EndDate(date_range))
  date_range2 <- DateRange("2012-01-01", "2012-01-01")
  DateRange(date_range2) <- date_range
  expect_equal(DateRange(date_range), date_range2)
})

test_that("SplitDateRange correctly splits a dateRange object, including that of a .query object", {
  # dateRange
  # query
})
