context("Parsing Google Analytics Core Reporting API responses")

test_that("", {

})

test_that("A basic query can be executed against the Google Store GA account", {
  check_api()
  q <- GaQuery(view = "1174")
  results <- GetGaData(q)
  expect_is(results, "data.frame")
  expect_equivalent(dim(results), c(8, 2))
  expect_named(results, c("date", "sessions"))
  expect_equivalent(sapply(results, class), c("Date", "numeric"))
