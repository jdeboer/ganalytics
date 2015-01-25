# API Error response codes: https://developers.google.com/analytics/devguides/config/mgmt/v3/errors

user_permission_levels <- c(
  "READ_AND_ANALYZE", "COLLABORATE", "EDIT", "MANAGE_USERS"
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

.gaManagementApi <- R6Class(
  public = list(
    creds = GaCreds(),
    get = function() {
      ga_api_request(
        creds = self$creds,
        request = c("management", self$.req_path),
        scope = private$scope
      )
    }
  ),
  private = list(
    request = NULL,
    scope = ga_scopes['read_only'],
    field_corrections = function(field_list) {
      if(is.data.frame(field_list)) {
        if(exists("created", field_list)) {
          field_list$created <- ymd_hms(field_list$created)
        }
        if(exists("updated", field_list)) {
          field_list$updated <- ymd_hms(field_list$updated)
        }
        subset(field_list, select = c(-kind, -selfLink, -childLink, -parentLink))
      } else {
        field_list
      }
    },
    parent_class_name = "NULL"
  )
)

.gaResource <- R6Class(
  ".gaResource",
  inherit = .gaManagementApi,
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
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      self$parent <- parent
      self$id <- id
      if(is.null(id)) {
        self
      } else {
        self$get()
      }
    },
    get = function() {
      if (!is.null(self$.req_path)) {
        response <- super$get()
        updated_fields <- private$field_corrections(response)
        self$modify(updated_fields)
      }
      self
    },
    .child_nodes = function(class_generator) {
      class_name <- class_generator$classname
      if (is(private$cache[[class_name]], class_name)) {
        private$cache[[class_name]]
      } else {
        private$cache[[class_name]] <- class_generator$new(parent = self)
      }
    }
  ),
  active = list(
    .req_path = function() {
      if (is.null(self$id)) {
        NULL
      } else {
        c(self$parent$.req_path, private$request, self$id)
      }
    }
  ),
  private = list(
    cache = list()
  )
)

.gaCollection <- R6Class(
  ".gaCollection",
  inherit = .gaManagementApi,
  public = list(
    summary = data.frame(),
    parent = NULL,
    get_entity = function(id) {
      entity <- private$entity_class$new(parent = self$parent, id = id)
      private$entities_cache[[id]] <- entity
      entity
    },
    get = function() {
      if (!is.null(self$.req_path)) {
        response <- super$get()
        self$summary <- private$field_corrections(response$items)
      }
      self
    },
    initialize = function(parent = NULL) {
      entity_class_private <- with(private$entity_class, c(private_fields, private_methods))
      private$request <- entity_class_private$request
      private$parent_class_name <- entity_class_private$parent_class_name
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      self$parent <- parent
      self$get()
    }
  ),
  active = list(
    entities = function() {
      if (is.data.frame(self$summary)) {
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
        return(ret)
      } else {
        return(NULL)
      }
    },
    .req_path = function() {
      # if this is top most level, e.g. 'Accounts' or "UserSegments", then there
      # is no parent and therefore there will not exist a parent request path,
      # i.e. it will be NULL. Otherwise, if there is a parent, but it has no
      # request path, then this should also not have a request path.
      if (!is.null(self$parent) & is.null(self$parent$.req_path)) {
        return(NULL)
      } else if (is(self$parent, private$parent_class_name)) {
        c(self$parent$.req_path, private$request)
      } else {
        return(NULL)
      }
    }
  ),
  private = list(
    entity_class = .gaResource,
    entities_cache = list()
  )
)

#' @export
gaUserSegment <- R6Class(
  "gaUserSegment",
  inherit = .gaResource,
  public = list(
    type = NA,
    segmentId = NA,
    definition = NA
  ),
  private = list(
    parent_class_name = "NULL",
    request = "segments"
  )
)

#' @export
gaUserSegments <- R6Class(
  "gaUserSegments",
  inherit = .gaCollection,
  private = list(
    entity_class = gaUserSegment
  )
)

#' @export
gaAccountSummary <- R6Class(
  "gaAccountSummary",
  inherit = .gaResource,
  private = list(
    parent_class_name = "NULL",
    request = "accountSummaries"
  )
)

#' @export
gaAccountSummaries <- R6Class(
  "gaAccountSummaries",
  inherit = .gaCollection,
  private = list(
    entity_class = gaAccountSummary
  )
)

#' @export
gaAccount <- R6Class(
  "gaAccount",
  inherit = .gaResource,
  public = list(
    get = function() {
      if (!is.null(self$.req_path)) {
        account_fields <- gaAccounts$new()$summary[self$id, ]
        self$modify(account_fields)
      }
      self
    }
  ),
  active = list(
    properties = function() {self$.child_nodes(gaProperties)},
    users = function() {
      tryCatch(
        self$.child_nodes(gaAccountUserLinks),
        error = function(e) {
          e$message
        }
      )
    }
  ),
  private = list(
    parent_class_name = "NULL",
    request = "accounts",
    field_corrections = function(field_list) {
      field_list <- super$field_corrections(field_list)
      mutate(
        field_list,
        permissions = lapply(permissions$effective, factor,
          levels = user_permission_levels
        )
      )
    }
  )
)

#' @export
gaAccounts <- R6Class(
  "gaAccounts",
  inherit = .gaCollection,
  private = list(
    entity_class = gaAccount,
    field_corrections = gaAccount$private_methods$field_corrections
  )
)

#' @export
gaViewFilter <- R6Class(
  "gaViewFilter",
  inherit = .gaResource,
  public = list(
    type = NA,
    details = NA
  ),
  private = list(
    parent_class_name = "gaAccount",
    request = "filters"
  )
)

#' @export
gaViewFilters <- R6Class(
  "gaViewFilters",
  inherit = .gaCollection,
  private = list(
    entity_class = gaViewFilter
  )
)

#' @export
gaAccountUserLink <- R6Class(
  "gaAccountUserLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaAccount",
    request = "entityUserLinks",
    scope = ga_scopes[c('read_only', 'manage_users')]#,
    #field_corrections = function(field_list) {field_list}
  )
)

#' @export
gaAccountUserLinks <- R6Class(
  "gaAccountUserLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaAccountUserLink,
    scope = gaAccountUserLink$private_fields$scope#,
    #field_corrections = gaAccountUserLink$private_methods$field_corrections
  )
)

#' @export
gaProperty <- R6Class(
  "gaProperty",
  inherit = .gaResource,
  public = list(
    websiteUrl = NA,
    industryVertical = NA,
    defaultViewId = NA
  ),
  active = list(
    views = function() {self$.child_nodes(gaViews)},
    defaultView = function() {
      self$views$entities[[self$defaultViewId]]
    },
    users = function() {
      tryCatch(
        self$.child_nodes(gaPropertyUserLinks),
        error = function(e) {
          e$message
        }
      )
    },
    adwordsLinks = function() {self$.child_nodes(gaAdwordsLinks)},
    dataSources = function() {self$.child_nodes(gaDataSources)}
  ),
  private = list(
    parent_class_name = "gaAccount",
    request = "webproperties",
    field_corrections = function(field_list) {
      if(is.data.frame(field_list)) {
        field_list <- super$field_corrections(field_list)
        field_list <- subset(field_list, select = c(-accountId, -internalWebPropertyId))
        rename(
          field_list,
          replace = c("defaultProfileId" = "defaultViewId")
        )
      } else {
        field_list
      }
    }
  )
)

#' @export
gaProperties <- R6Class(
  "gaProperties",
  inherit = .gaCollection,
  private = list(
    entity_class = gaProperty,
    field_corrections = gaProperty$private_methods$field_corrections
  )
)

#' @export
gaPropertyUserLink <- R6Class(
  "gaPropertyUserLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaProperty",
    request = "entityUserLinks",
    scope = ga_scopes[c('read_only', 'manage_users')]#,
    #field_corrections = function(field_list) {field_list}
  )
)

#' @export
gaPropertyUserLinks <- R6Class(
  "gaPropertyUserLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaPropertyUserLink,
    scope = gaPropertyUserLink$private_fields$scope#,
    #field_corrections = gaPropertyUserLink$private_methods$field_corrections
  )
)

#' @export
gaAdwordsLink <- R6Class(
  "gaAdwordsLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaProperty",
    request = "entityAdWordsLinks"
  )
)

#' @export
gaAdwordsLinks <- R6Class(
  "gaAdwordsLinks",
  inherit = .gaCollection,
  public = list(
    adWordsAccounts = NA
  ),
  private = list(
    entity_class = gaAdwordsLink
  )
)

#' @export
gaDataSource <- R6Class(
  "gaDataSource",
  inherit = .gaResource,
  public = list(
    description = NA,
    type = NA,
    importBehavior = NA,
    profilesLinked = NA
  ),
  active = list(
    uploads = function(){self$.child_nodes(gaUploads)}
  ),
  private = list(
    parent_class_name = "gaProperty",
    request = "customDataSources"
  )
)

#' @export
gaDataSources <- R6Class(
  "gaDataSources",
  inherit = .gaCollection,
  private = list(
    entity_class = gaDataSource
  )
)

#' @export
gaUpload <- R6Class(
  "gaUpload",
  inherit = .gaResource,
  public = list(
    status = NA,
    errors = NA
  ),
  private = list(
    parent_class_name = "gaDataSource",
    request = "uploads"
  )
)

#' @export
gaUploads <- R6Class(
  "gaUploads",
  inherit = .gaCollection,
  private = list(
    entity_class = gaUpload
  )
)

#' @export
gaView <- R6Class(
  "gaView",
  inherit = .gaResource,
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
    goals = function() {self$.child_nodes(gaGoals)},
    experiments = function() {self$.child_nodes(gaExperiments)},
    unsampledReports = function() {self$.child_nodes(gaUnsampledReports)},
    users = function() {
      tryCatch(
        self$.child_nodes(gaViewUserLinks),
        error = function(e) {
          e$message
        }
      )
    },
    viewFilterLinks = function() {self$.child_nodes(gaViewFilterLinks)}
  ),
  private = list(
    parent_class_name = "gaProperty",
    request = "profiles",
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
gaViews <- R6Class(
  "gaViews",
  inherit = .gaCollection,
  private = list(
    entity_class = gaView,
    field_corrections = gaView$private_methods$field_corrections
  )
)

#' @export
gaGoal <- R6Class(
  "gaGoal",
  inherit = .gaResource,
  public = list(
    type = NA,
    value = NA,
    active = NA,
    details = NA
  ),
  private = list(
    parent_class_name = "gaView",
    request = "goals"#,
    #field_corrections = function(field_list) {field_list}
  )
)

#' @export
gaGoals <- R6Class(
  "gaGoals",
  inherit = .gaCollection,
  private = list(
    entity_class = gaGoal#,
    #field_corrections = gaGoal$private_methods$field_corrections
  )
)

#' @export
gaExperiment <- R6Class(
  "gaExperiment",
  inherit = .gaResource,
  public = list(
    description = NA,
    objectiveMetric = NA,
    optimizationType = NA,
    status = NA,
    winnerFound = NA,
    startTime = NA,
    endTime = NA,
    reasonExperimentEnded = NA,
    rewriteVariationUrlsAsOriginal = NA,
    winnerConfidenceLevel = NA,
    minimumExperimentLengthInDays = NA,
    trafficCoverage = NA,
    equalWeighting = NA,
    servingFramework = NA,
    variations = NA
  ),
  private = list(
    parent_class_name = "gaView",
    request = "experiments"
  )
)

#' @export
gaExperiments <- R6Class(
  "gaExperiments",
  inherit = .gaCollection,
  private = list(
    entity_class = gaExperiment
  )
)

#' @export
gaUnsampledReport <- R6Class(
  "gaUnsampledReport",
  inherit = .gaResource,
  public = list(
    title = NA,
    startDate = NA,
    endDate = NA,
    metrics = NA,
    dimensions = NA,
    filters = NA,
    segment = NA,
    status = NA,
    downloadType = NA,
    driveDownloadDetails = NA,
    cloudStorageDownloadDetails = NA
  ),
  private = list(
    parent_class_name = "gaView",
    request = "unsampledReports"
  )
)

#' @export
gaUnsampledReports <- R6Class(
  "gaUnsampledReports",
  inherit = .gaCollection,
  private = list(
    entity_class = gaUnsampledReport
  )
)

#' @export
gaViewUserLink <- R6Class(
  "gaViewUserLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaView",
    request = "entityUserLinks",
    scope = ga_scopes[c('read_only', 'manage_users')]#,
    #field_corrections = function(field_list) {field_list}
  )
)

#' @export
gaViewUserLinks <- R6Class(
  "gaViewUserLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaViewUserLink,
    scope = gaViewUserLink$private_fields$scope#,
    #field_corrections = gaViewUserLink$private_methods$field_corrections
  )
)

#' @export
gaViewFilterLink <- R6Class(
  "gaViewFilterLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaView",
    request = "profileFilterLinks"#,
    #field_corrections = function(field_list) {field_list}
  )
)

#' @export
gaViewFilterLinks <- R6Class(
  "gaViewFilterLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaViewFilterLink#,
    #field_corrections = gaViewFilterLink$private_methods$field_corrections
  )
)

