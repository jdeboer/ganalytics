#' @include utils.R
#' @importFrom assertthat assert_that
#' @importFrom stringr str_split_fixed
#' @importFrom methods as setAs
NULL

# Coercion to Date
setAs(from = "character", to = "Date",
      def = function(from) {
        as.Date(
          parse_date(from, output_format = kGaDateInFormat),
          format = kGaDateInFormat
        )
      }
)

# Coercion to dateRange
setAs(from = "Date", to = "dateRange",
      def = function(from, to) {
        assert_that(length(from) == 2L)
        startDate = from[1L]
        endDate = from[2L]
        DateRange(startDate, endDate)
      }
)

setAs(from = "Interval", to = "dateRange", def = simpleCoerce)

