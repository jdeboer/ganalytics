#'@include helper-functions.R
# Google Analytics Management API classes

setClass(
  "Resource",
  slots = list(
    id = "character", name = "character",
    created = "POSIXt", updated = "POSIXt",
    request = "character"
  ),
  prototype = prototype(
    created = now(), updated = now(),
    request = ""
  ),
  validity = function(object) {
    CheckVectorBounds(object, list(
      name = c(1, 1),
      id = c(0, 1),
      created = c(0, 1),
      updated = c(0, 1),
      request = c(1, 1)
    ))
  }
)

setClass(
  "Collection",
  slots = list(
    resource_class = "character",
    request = "character"
  ),
  prototype = prototype(
    resource_class = "Resource"
  ),
  contains = "list",
  validity = function(object) {
    if (length(object@resource_class) != 1) {
      return("Slot resource_class must be of length 1.")
    }
    if (any(sapply(object@.Data, class) != object@resource_class)) {
      return(
        paste0("All items in the list must be of class '", object@resource_class, "'")
      )
    }
    TRUE
  }
)

setClass(
  "Account",
  slots = list(
    belongs_to = "NULL"
  ),
  prototype = prototype(
    request = "accounts"
  ),
  contains = "Resource"
)

setClass(
  "Accounts",
  slots = list(
    belongs_to = "NULL"
  ),
  prototype = prototype(
    resource_class = "Account",
    request = "accounts"
  ),
  contains = "Collection"
)

setClass(
  "Property",
  slots = list(
    belongs_to = "Account",
    websiteUrl = "character",
    industryVertical = "character",
    defaultView = "character"
  ),
  prototype = prototype(
    request = "webproperties",
    websiteUrl = character(),
    industryVertical = character(),
    defaultView = character()
  ),
  contains = "Resource",
  validity = function(object) {
    CheckVectorBounds(object, list(
      belongs_to = c(1, 1),
      websiteUrl = c(0, 1),
      industryVertical = c(0, 1),
      defaultView = c(0, 1)
    ))
  }
)

setClass(
  "Properties",
  slots = list(
    belongs_to = "Account"
  ),
  prototype = prototype(
    resource_class = "Property",
    request = "webproperties"
  ),
  contains = "Collection"
)

view_type_levels <- c("WEB", "APP")

currency_levels <- c(
  "ARS", "AUD", "BGN", "BRL", "CAD", "CHF",
  "CNY", "CZK", "DKK", "EUR", "GBP", "HKD",
  "HUF", "IDR", "INR", "JPY", "KRW", "LTL",
  "MXN", "NOK", "NZD", "PHP", "PLN", "RUB",
  "SEK", "THB", "TRY", "TWD", "USD", "VND", "ZAR"
)

setClass(
  "View",
  slots = list(
    belongs_to = "Property",
    type = "factor",
    currency = "factor",
    timezone = "character",
    websiteUrl = "character",
    defaultPage = "character",
    excludeQueryParameters = "character",
    siteSearchQueryParameters = "character",
    stripSiteSearchQueryParameters = "logical",
    siteSearchCategoryParameters = "character",
    stripSiteSearchCategoryParameters = "logical",
    eCommerceTracking = "logical"
  ),
  prototype = prototype(
    request = "profiles",
    type = factor("WEB", levels = view_type_levels),
    currency = factor("AUD", levels = currency_levels),
    timezone = Sys.timezone(),
    websiteUrl = character(),
    defaultPage = character(),
    excludeQueryParameters = character(),
    siteSearchQueryParameters = character(),
    stripSiteSearchQueryParameters = FALSE,
    siteSearchCategoryParameters = character(),
    stripSiteSearchCategoryParameters = FALSE,
    eCommerceTracking = FALSE
  ),
  contains = "Resource",
  validity = function(object) {
    CheckVectorBounds(object, list(
      belongs_to = c(1, 1),
      type = c(1, 1),
      currency = c(0, 1),
      timezone = c(0, 1),
      websiteUrl = c(0, 1),
      defaultPage = c(0, 1),
      excludeQueryParameters = c(0, Inf),
      siteSearchQueryParameters = c(0, Inf),
      stripSiteSearchQueryParameters = c(1, 1),
      siteSearchCategoryParameters = c(0, Inf),
      stripSiteSearchCategoryParameters = c(1, 1),
      eCommerceTracking = c(1, 1)
    ))
  }
)

setClass(
  "Views",
  slots = list(
    belongs_to = "Property"
  ),
  prototype = prototype(
    resource_class = "View",
    request = "profiles"
  ),
  contains = "Collection"
)

# setClass(
#   "User_Ref",
#   slots = list(
#     id = "character",
#     email = "character"
#   ),
#   prototype = prototype(
#     request = "entityUserLinks"
#   )
# )
# 
# setClass(
#   "Account_User_Link",
#   slots = list(
#     id = "character",
#     userRef = "User_Ref",
#     account = "Account"
#   ),
#   prototype = prototype(
#     request = "entityUserLinks"
#   )
# )
# 
# setClass(
#   "Property_User_Link",
#   slots = list(
#     id = "character",
#     userRef = "User_Ref",
#     property = "Property"
#   ),
#   prototype = prototype(
#     request = "entityUserLinks"
#   )
# )
# 
# setClass(
#   "View_User_Link",
#   slots = list(
#     id = "character",
#     userRef = "User_Ref",
#     view = "View"
#   ),
#   prototype = prototype(
#     request = "entityUserLinks"
#   )
# )
