#' @include utils.R
#' @importFrom assertthat assert_that
#' @importFrom stringr str_split_fixed
NULL

#############\/ Transform to method of DateRange and DateRange<- generic functions

# Coercion to Date
setAs(from = "character", to = "Date",
      def = function(from) {
        as.Date(parse_date(from, output_format = kGaDateInFormat), format = kGaDateInFormat)
      }
)

# Coercion to dateRange
setAs(from = "Date", to = "dateRange",
      def = function(from, to) {
        assert_that(length(from) == 2)
        startDate = from[1]
        endDate = from[2]
        new("dateRange", startDate, endDate)
      }
)

setAs(from = "Interval", to = "dateRange",
      def = function(from, to) {
        date_range_char <- str_split_fixed(as.character(from), "--", 2)
        start_date <- as.Date(date_range_char[, 1])
        end_date <- as.Date(date_range_char[, 2])
        if (start_date <= end_date) {
          new(to, start_date, end_date)
        } else {
          new(to, end_date, start_date)
        }
      }
)

setAs(from = ".query", to = "dateRange",
      def = function(from, to) {
        from@dateRange
      },
      replace = function(from, value) {
        from@dateRange <- as(value, "dateRange")
        validObject(from)
        from
      }
)
