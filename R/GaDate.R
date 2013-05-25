# -- GaDateRange ----
setMethod(
  f = "GaDateRange",
  signature = "gaDateRange",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaDateRange",
  signature = c("character", "character"),
  definition = function(.Object, endDate) {
    startDate <- as.Date(.Object, format = kGaDateInFormat)
    endDate <- as.Date(endDate, format = kGaDateInFormat)
    new("gaDateRange", startDate, endDate)
  }
)

setMethod(
  f = "GaDateRange",
  signature = c("Date", "Date"),
  definition = function(.Object, endDate) {
    startDate <- .Object
    new("gaDateRange", startDate, endDate)
  }
)

setMethod(
  f = "GaStartDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(.Object, format = kGaDateInFormat)
  }
)

setMethod(
  f = "GaEndDate",
  signature = "character",
  definition = function(.Object) {
    as.Date(.Object, format = kGaDateInFormat)
  }
)

setMethod(
  f = "GaStartDate",
  signature = "gaDateRange",
  definition = function(.Object) {
    .Object@startDate
  }
)

setMethod(
  f = "GaEndDate",
  signature = "gaDateRange",
  definition = function(.Object) {
    .Object@endDate
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaDateRange", "character"),
  definition = function(.Object, value) {
    startDate <- as.Date(value, format = kGaDateInFormat)
    endDate <- GaEndDate(.Object)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaDateRange", "character"),
  definition = function(.Object, value) {
    startDate <- GaStartDate(.Object)
    endDate <- as.Date(value, format = kGaDateInFormat)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaDateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- as.Date(value, format = kGaDateInFormat)
    endDate <- GaEndDate(.Object)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaDateRange", "Date"),
  definition = function(.Object, value) {
    startDate <- GaStartDate(.Object)
    endDate <- as.Date(value, format = kGaDateInFormat)
    GaDateRange(startDate, endDate)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("gaDateRange", "character"),
  definition = function(.Object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(value[1], format = kGaDateInFormat)
      endDate <- as.Date(value[2], format = kGaDateInFormat)
      new("gaDateRange", startDate, endDate)
    }
  }
)


setMethod(
  f = "GaDateRange",
  signature = "gaQuery",
  definition = function(.Object) {
    return(.Object@dateRange)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("gaQuery", "gaDateRange"),
  definition = function(.Object, value) {
    .Object@dateRange <- value
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  f = "GaDateRange<-",
  signature = c("gaQuery", "character"),
  definition = function(.Object, value) {
    if (length(value) != 2) {
      stop("value must contain the start date and end date in a character vecotr of legnth 2.")
    } else {
      startDate <- as.Date(value[1], format = kGaDateInFormat)
      endDate <- as.Date(value[2], format = kGaDateInFormat)
      newDateRange <- new("gaDateRange", startDate, endDate)
      GaDateRange(.Object) <- newDateRange
      return(.Object)
    }
  }
)


setMethod(
  f = "GaStartDate",
  signature = "gaQuery",
  definition = function(.Object) {
    GaStartDate(.Object@dateRange)
  }
)

setMethod(
  f = "GaEndDate",
  signature = "gaQuery",
  definition = function(.Object) {
    GaEndDate(.Object@dateRange)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaQuery", "character"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaStartDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaQuery", "character"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaEndDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

setMethod(
  f = "GaStartDate<-",
  signature = c("gaQuery", "Date"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaStartDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

setMethod(
  f = "GaEndDate<-",
  signature = c("gaQuery", "Date"),
  definition = function(.Object, value) {
    dateRange <- GaDateRange(.Object)
    GaEndDate(dateRange) <- value
    GaDateRange(.Object) <- dateRange
    return(.Object)
  }
)

