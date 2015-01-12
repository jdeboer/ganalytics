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
  "APP_ID", "APP_INSTALLER_ID", "APP_NAME", "APP_VERSION", "SCREEN",
  "IS_APP", "IS_FATAL_EXCEPTION", "EXCEPTION_DESCRIPTION",
  "IS_MOBILE", "IS_TABLET",
  "MOBILE_HAS_QWERTY_KEYBOARD", "MOBILE_HAS_NFC_SUPPORT",
  "MOBILE_HAS_CELLULAR_RADIO", "MOBILE_HAS_WIFI_SUPPORT",
  "MOBILE_BRAND_NAME", "MOBILE_MODEL_NAME",
  "MOBILE_MARKETING_NAME", "MOBILE_POINTING_METHOD",
  "SOCIAL_NETWORK", "SOCIAL_ACTION", "SOCIAL_ACTION_TARGET"
)

include_exclude_filter_match_type_levels <- c(
  "BEGINS_WITH", "EQUAL", "ENDS_WITH", "CONTAINS", "MATCHES"
)

.GaManagementApi <- R6Class(
  public = list(
    creds = GaCreds()
  ),
  active = list(
    req_path = function() {}
  )
)

.GaResource <- R6Class(
  ".GaResource",
  inherit = .GaManagementApi,
  public = list(
    id = NA,
    name = NA,
    created = NA,
    updated = NA,
    parent = NULL,
    modify = function(field_list) {
      l_ply(names(field_list), function(field_name) {
        if (exists(field_name, self)) {
          self[[field_name]] <- field_list[[field_name]]
        }
      })
      self
    },
    initialize = function(parent = NULL, id = NULL) {
      stopifnot(is(parent, private$parent_class_name))
      self$parent <- parent
      self$id <- id
      if(is.null(id)) {
        self
      } else {
        self$get()
      }
    },
    get = function() {
      if (!is.null(self$req_path)) {
        scope <- ga_scopes['read_only']
        response <- ga_api_request(
          creds = self$creds,
          request = c("management", self$req_path),
          scope = scope
        )
        updated_fields <- private$field_corrections(response)
        self$modify(updated_fields)
      }
      self
    }
  ),
  active = list(
    req_path = function() {
      if (is.null(self$id)) {
        NULL
      } else {
        c(self$parent$req_path, private$request, self$id)
      }
    }
  ),
  private = list(
    parent_class_name = "NULL",
    request = NULL,
    field_corrections = function(field_list) {
      mutate(
        field_list,
        created = ymd_hms(field_list$created),
        updated = ymd_hms(field_list$updated)
      )
    }
  )
)

.GaCollection <- R6Class(
  ".GaCollection",
  inherit = .GaManagementApi,
  public = list(
    summary = data.frame(),
    parent = NULL,
    get_entity = function(id) {
      entity <- private$entity_class$new(parent = self$parent, id = id)
      private$entities_cache[[id]] <- entity
      entity
    },
    get = function() {
      if (!is.null(self$req_path)) {
        scope <- ga_scopes['read_only']
        response <- ga_api_request(
          creds = self$creds,
          request = c("management", self$req_path),
          scope = scope
        )
        self$summary <- private$field_corrections(response$items)
        rownames(self$summary) <- self$summary$id
      }
      self
    },
    initialize = function(parent = NULL) {
      private$request <- private$entity_class$private_fields$request
      private$parent_class_name <- private$entity_class$private_fields$parent_class_name
      stopifnot(is(parent, private$parent_class_name))
      self$parent <- parent
      self$get()
    }
  ),
  active = list(
    entities = function() {
      ret <- alply(self$summary, 1, function(summary_row) {
        field_list <- as.list(summary_row)
        id <- summary_row$id
        updated <- summary_row$updated
        entity <- private$entities_cache[[id]]
        if (
          !is(entity, private$entity_class$classname) |
            identical(entity$updated != updated, TRUE)
        ) {
          entity <- private$entity_class$new(parent = self$parent)
          entity$modify(field_list = field_list)
          private$entities_cache[[id]] <- entity
        }
        entity
      })
      attributes(ret) <- NULL
      names(ret) <- self$summary$id
      ret
    },
    req_path = function() {
      # if this is top most level, e.g. 'Accounts' or "UserSegments", then there
      # is no parent and therefore there will not exist a parent request path,
      # i.e. it will be NULL. Otherwise, if there is a parent, but it has no
      # request path, then this should also not have a request path.
      if (!is.null(self$parent) & is.null(self$parent$req_path)) {
        return(NULL)
      } else {
        c(self$parent$req_path, private$request)
      }
    }
  ),
  private = list(
    request = NULL,
    parent_class_name = NULL,
    entity_class = .GaResource,
    field_corrections = .GaResource$private_methods$field_corrections,
    entities_cache = list()
  )
)

#' @export
GaUserSegment <- R6Class(
  "GaUserSegment",
  inherit = .GaResource,
  private = list(
    parent_class_name = "NULL",
    request = "segments"
  )
)

#' @export
GaUserSegments <- R6Class(
  "GaUserSegments",
  inherit = .GaCollection,
  private = list(
    entity_class = GaUserSegment
  )
)

#' @export
GaAccountSummary <- R6Class(
  "GaAccountSummary",
  inherit = .GaResource,
  private = list(
    parent_class_name = "NULL",
    request = "accountSummaries"
  )
)

#' @export
GaAccountSummaries <- R6Class(
  "GaAccountSummaries",
  inherit = .GaCollection,
  private = list(
    entity_class = GaAccountSummary
  )
)

#' @export
GaAccount <- R6Class(
  "GaAccount",
  inherit = .GaResource,
  public = list(
    get = function() {
      if (!is.null(self$req_path)) {
        account_fields <- GaAccounts$new()$summary[self$id, ]
        self$modify(account_fields)
      }
      self
    }
  ),
  active = list(
    properties = function() {
      if (is(private$properties_cache, "GaProperties")) {
        private$properties_cache
      } else {
        private$properties_cache <- GaProperties$new(parent = self)
      } 
    }
  ),
  private = list(
    parent_class_name = "NULL",
    request = "accounts",
    properties_cache = list()
  )
)

#' @export
GaAccounts <- R6Class(
  "GaAccounts",
  inherit = .GaCollection,
  private = list(
    entity_class = GaAccount
  )
)

#' @export
GaFilter <- R6Class(
  "GaFilter",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaAccount",
    request = "filters"
  )
)

#' @export
GaFilters <- R6Class(
  "GaFilters",
  inherit = .GaCollection,
  private = list(
    entity_class = GaFilter
  )
)

#' @export
GaAccountUserLink <- R6Class(
  "GaAccountUserLink",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaAccount",
    request = "entityUserLinks"
  )
)

#' @export
GaAccountUserLinks <- R6Class(
  "GaAccountUserLinks",
  inherit = .GaCollection,
  private = list(
    entity_class = GaAccountUserLink
  )
)

#' @export
GaProperty <- R6Class(
  "GaProperty",
  inherit = .GaResource,
  public = list(
    websiteUrl = NA,
    industryVertical = NA,
    defaultViewId = NA
  ),
  active = list(
    views = function() {
      if (is(private$views_cache, "GaViews")) {
        private$views_cache
      } else {
        private$views_cache <- GaViews$new(parent = self)
      } 
    },
    defaultView = function() {
      self$views$entities[[self$defaultViewId]]
    }
  ),
  private = list(
    parent_class_name = "GaAccount",
    request = "webproperties",
    views_cache = list(),
    field_corrections = function(field_list) {
      field_list <- super$field_corrections(field_list)
      rename(
        field_list,
        replace = c("defaultProfileId" = "defaultViewId")
      )
    }
  )
)

#' @export
GaProperties <- R6Class(
  "GaProperties",
  inherit = .GaCollection,
  private = list(
    entity_class = GaProperty
  )
)

#' @export
GaPropertyUserLink <- R6Class(
  "GaPropertyUserLink",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaProperty",
    request = "entityUserLinks"
  )
)

#' @export
GaPropertyUserLinks <- R6Class(
  "GaPropertyUserLinks",
  inherit = .GaCollection,
  private = list(
    entity_class = GaPropertyUserLink
  )
)

#' @export
GaAdwordsLink <- R6Class(
  "GaAdwordsLink",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaProperty",
    request = "entityAdWordsLinks"
  )
)

#' @export
GaAdwordsLinks <- R6Class(
  "GaAdwordsLinks",
  inherit = .GaCollection,
  private = list(
    entity_class = GaAdwordsLink
  )
)

#' @export
GaDataSource <- R6Class(
  "GaDataSource",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaProperty",
    request = "customDataSources"
  )
)

#' @export
GaDataSources <- R6Class(
  "GaDataSources",
  inherit = .GaCollection,
  private = list(
    entity_class = GaDataSource
  )
)

#' @export
GaUpload <- R6Class(
  "GaUpload",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaDataSource",
    request = "uploads"
  )
)

#' @export
GaUploads <- R6Class(
  "GaUploads",
  inherit = .GaCollection,
  private = list(
    entity_class = GaUpload
  )
)

#' @export
GaView <- R6Class(
  "GaView",
  inherit = .GaResource,
  public = list(
    type = NA,
    currency = NA,
    timezone = NA,
    websiteUrl = NA,
    defaultPage = NA,
    excludeQueryParameters = NA,
    siteSearchQueryParameters = NA,
    stripSiteSearchQueryParameters = NA,
    siteSearchCategoryParameters = NA,
    stripSiteSearchCategoryParameters = NA,
    eCommerceTracking = NA,
    enhancedECommerceTracking = NA
  ),
  active = list(
    goals = function() {
      if (is(private$goals_cache, "GaGoals")) {
        private$goals_cache
      } else {
        private$goals_cache <- GaGoals$new(parent = self)
      } 
    },
    experiments = function() {
      if (is(private$experiments_cache, "GaExperiments")) {
        private$experiments_cache
      } else {
        private$experiments_cache <- GaExperiments$new(parent = self)
      }
    },
    unsampledReports = function() {
      if (is(private$unsampledReports_cache, "GaUnsampledReports")) {
        private$unsampledReports_cache
      } else {
        private$unsampledReports_cache <- GaUnsampledReports$new(parent = self)
      }
    },
    viewUserLinks = function() {
      if (is(private$viewUserLinks_cache, "GaViewUserLinks")) {
        private$viewUserLinks_cache
      } else {
        private$viewUserLinks_cache <- GaViewUserLinks$new(parent = self)
      }
    },
    viewFilterLinks = function() {
      if (is(private$viewFilterLinks_cache, "GaViewFilterLinks")) {
        private$viewFilterLinks_cache
      } else {
        private$viewFilterLinks_cache <- GaViewFilterLinks$new(parent = self)
      }
    }
  ),
  private = list(
    parent_class_name = "GaProperty",
    request = "profiles",
    goals_cache = list(),
    experiments_cache = list(),
    unsampledReports_cache = list(),
    field_corrections = function(field_list) {
      field_list <- super$field_corrections(field_list)
      mutate(
        field_list,
        type = factor(type, levels = view_type_levels),
        currency = factor(currency, levels = currency_levels),
        stripSiteSearchQueryParameters = identical(field_list$stripSiteSearchQueryParameters, TRUE),
        stripSiteSearchCategoryParameters = identical(field_list$stripSiteSearchCategoryParameters, TRUE)        
      )
    }
  )
)

#' @export
GaViews <- R6Class(
  "GaViews",
  inherit = .GaCollection,
  private = list(
    entity_class = GaView
  )
)

#' @export
GaGoal <- R6Class(
  "GaGoal",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaView",
    request = "goals"
  )
)

#' @export
GaGoals <- R6Class(
  "GaGoals",
  inherit = .GaCollection,
  private = list(
    entity_class = GaGoal
  )
)

#' @export
GaExperiment <- R6Class(
  "GaExperiment",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaView",
    request = "experiments"
  )
)

#' @export
GaExperiments <- R6Class(
  "GaExperiments",
  inherit = .GaCollection,
  private = list(
    entity_class = GaExperiment
  )
)

#' @export
GaUnsampledReport <- R6Class(
  "GaUnsampledReport",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaView",
    request = "unsampledReports"
  )
)

#' @export
GaUnsampledReports <- R6Class(
  "GaUnsampledReports",
  inherit = .GaCollection,
  private = list(
    entity_class = GaUnsampledReport
  )
)

#' @export
GaViewUserLink <- R6Class(
  "GaViewUserLink",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaView",
    request = "entityUserLinks"
  )
)

#' @export
GaViewUserLinks <- R6Class(
  "GaViewUserLinks",
  inherit = .GaCollection,
  private = list(
    entity_class = GaViewUserLink
  )
)

#' @export
GaViewFilterLink <- R6Class(
  "GaViewFilterLink",
  inherit = .GaResource,
  private = list(
    parent_class_name = "GaView",
    request = "profileFilterLinks"
  )
)

#' @export
GaViewFilterLinks <- R6Class(
  "GaViewFilterLinks",
  inherit = .GaCollection,
  private = list(
    entity_class = GaViewFilterLink
  )
)

