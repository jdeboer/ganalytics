ga_scopes <- c(
  default = "https://www.googleapis.com/auth/analytics",
  edit = "https://www.googleapis.com/auth/analytics.edit",
  read_only = "https://www.googleapis.com/auth/analytics.readonly",
  manage_users = "https://www.googleapis.com/auth/analytics.manage.users",
  read_only_users = "https://www.googleapis.com/auth/analytics.manage.users.readonly"
)

gtm_scopes <- c(
  read_only = "https://www.googleapis.com/auth/tagmanager.readonly",
  edit_containers = "https://www.googleapis.com/auth/tagmanager.edit.containers",
  delete_containers = "https://www.googleapis.com/auth/tagmanager.delete.containers",
  edit_container_versions = "https://www.googleapis.com/auth/tagmanager.edit.containerversions",
  publish = "https://www.googleapis.com/auth/tagmanager.publish",
  manage_users = "https://www.googleapis.com/auth/tagmanager.manage.users",
  manage_accounts = "https://www.googleapis.com/auth/tagmanager.manage.accounts"
)

kGaDayOfWeek <- c(
  "Sun",
  "Mon",
  "Tue",
  "Wed",
  "Thu",
  "Fri",
  "Sat"
)

# Google Analytics dimension types
kGaDimTypes <- list(
  dates = c(
    "dateOfSession",
    "ga:date",
    "ga:dateHour",
    "ga:socialActivityTimestamp"
  ),
  orderedIntFactors = c(
    "ga:year",
    "ga:isoYear",
    "ga:month",
    "ga:week",
    "ga:isoWeek",
    "ga:day",
    "ga:dayOfWeek",
    "ga:hour",
    "ga:minute",
    "ga:yearMonth",
    "ga:yearWeek",
    "ga:isoYearIsoWeek"
  ),
  orderedOtherFactors = c(
    "ga:screenColors",
    "ga:screenResolution",
    "ga:userAgeBracket",
    "ga:visitorAgeBracket",
    "ga:dayOfWeekName"
  ),
  nums = c(
    "ga:latitude",
    "ga:longitude",
    "ga:visitLength",
    "ga:pageDepth",
    "ga:screenDepth",
    "ga:sessionDurationBucket",
    "ga:daysSinceLastVisit",
    "ga:daysSinceLastSession",
    "ga:sessionsToTransaction",
    "ga:visitsToTransaction",
    "ga:daysToTransaction",
    "ga:internalPromotionPosition",
    "ga:productListPosition",
    "ga:visitCount",
    "ga:sessionCount",
    "ga:nthMinute",
    "ga:nthHour",
    "ga:nthDay",
    "ga:nthWeek",
    "ga:nthMonth"
  ),
  bools = c(
    "ga:isMobile",
    "ga:isTablet",
    "ga:javaEnabled",
    "ga:searchUsed",
    "ga:isTrueViewVideoAd",
    "ga:hasSocialSourceReferral"
  )
)

samplingLevel_levels <- c("DEFAULT", "FASTER", "HIGHER_PRECISION")

# Constants
# ---------

# Google Analytics date formats
kGaDateInFormat <- "%Y-%m-%d"
kGaDateOutFormat <- "%Y%m%d"

# The earliest valid date is 20050101. There is no upper limit restriction for a start-date.
kGaDateOrigin <- as.Date("2005-01-01")

# Google Analytics expression comparators
kGaOps <- list(
  met = c("==", "!=", "<", ">", "<=", ">=", "<>"),
  dim = c("==", "!=", "=~", "!~", "=@", "!@", "<>", "[]")
)

kMcfOps <- list(
  met = c("==", "!=", "<", ">", "<=", ">="),
  dim = c("==", "!=", "=~", "!~", "=@", "!@")
)

kRtOps <- list(
  met = c("==", "!=", "<", ">", "<=", ">="),
  dim = c("==", "!=", "=~", "!~", "=@", "!@")
)

# ganalytics tolerated dimension and metric prefixes
kPrefixDelim <- paste0("[", paste0(c(
  ";", "\\-", "\\.", "_"),
collapse = ""), "]")
kGaPrefix <- paste0("^ga", kPrefixDelim)
kMcfPrefix <- paste0("^mcf", kPrefixDelim)
kRtPrefix <- paste0("^rt", kPrefixDelim)

# Maximum dimensions and metrics allowed by Google Analytics Core Reporting API
kGaMax <- list(
  dimensions = 7,
  metrics = 10
)

# Maximum results per page and maximum rows accessible in a query.
kGaMaxResults <- 10000L
kGaMaxRows <- 1000000L

user_permission_levels <- c(
  "READ_AND_ANALYZE", "COLLABORATE", "EDIT", "MANAGE_USERS"
)

gtm_container_permission_levels <- c(
  "read", "edit", "delete", "publish"
)

view_type_levels <- c("WEB", "APP")

currency_levels <- c(
  "ARS", "AUD", "BGN", "BRL", "CAD", "CHF",
  "CNY", "CZK", "DKK", "EUR", "GBP", "HKD",
  "HUF", "IDR", "INR", "JPY", "KRW", "LTL",
  "MXN", "NOK", "NZD", "PHP", "PLN", "RUB",
  "SEK", "THB", "TRY", "TWD", "USD", "VND", "ZAR"
)

filter_type_levels <- c(
  "INCLUDE", "EXCLUDE",
  "LOWERCASE", "UPPERCASE",
  "SEARCH_AND_REPLACE", "ADVANCED"
)

filter_field_levels <- c(
  "UNUSED",
  "PAGE_REQUEST_URI", "PAGE_HOSTNAME", "PAGE_TITLE",
  "REFERRAL", "COST_DATA_URI", "HIT_TYPE",
  "INTERNAL_SEARCH_TERM", "INTERNAL_SEARCH_TYPE",
  "SOURCE_PROPERTY_TRACKING_ID",
  "CAMPAIGN_SOURCE", "CAMPAIGN_MEDIUM", "CAMPAIGN_NAME", "CAMPAIGN_AD_GROUP",
  "CAMPAIGN_TERM", "CAMPAIGN_CONTENT", "CAMPAIGN_CODE", "CAMPAIGN_REFERRAL_PATH",
  "TRANSACTION_COUNTRY", "TRANSACTION_REGION",
  "TRANSACTION_CITY", "TRANSACTION_AFFILIATION",
  "ITEM_NAME", "ITEM_CODE", "ITEM_VARIATION",
  "TRANSACTION_ID", "TRANSACTION_CURRENCY_CODE",
  "PRODUCT_ACTION_TYPE",
  "BROWSER", "BROWSER_VERSION", "BROWSER_SIZE", "PLATFORM",
  "PLATFORM_VERSION", "LANGUAGE", "SCREEN_RESOLUTION", "SCREEN_COLORS",
  "JAVA_ENABLED", "FLASH_VERSION", "GEO_SPEED", "VISITOR_TYPE",
  "GEO_ORGANIZATION", "GEO_DOMAIN", "GEO_IP_ADDRESS", "GEO_IP_VERSION",
  "GEO_COUNTRY", "GEO_REGION", "GEO_CITY",
  "EVENT_CATEGORY", "EVENT_ACTION", "EVENT_LABEL",
  "CUSTOM_FIELD_1", "CUSTOM_FIELD_2", "USER_DEFINED_VALUE",
  "CUSTOM_DIMENSIONS",
  "APP_ID", "APP_INSTALLER_ID", "APP_NAME", "APP_VERSION", "SCREEN",
  "IS_APP", "IS_FATAL_EXCEPTION", "EXCEPTION_DESCRIPTION",
  "DEVICE_CATEGORY",
  "MOBILE_HAS_QWERTY_KEYBOARD", "MOBILE_HAS_NFC_SUPPORT",
  "MOBILE_HAS_CELLULAR_RADIO", "MOBILE_HAS_WIFI_SUPPORT",
  "MOBILE_BRAND_NAME", "MOBILE_MODEL_NAME",
  "MOBILE_MARKETING_NAME", "MOBILE_POINTING_METHOD",
  "SOCIAL_NETWORK", "SOCIAL_ACTION", "SOCIAL_ACTION_TARGET"
)

include_exclude_filter_match_type_levels <- c(
  "BEGINS_WITH", "EQUAL", "ENDS_WITH", "CONTAINS", "MATCHES"
)

user_segment_type_levels <- c(
  "BUILT_IN", "CUSTOM"
)

