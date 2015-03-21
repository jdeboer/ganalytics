#'@include ganalytics-package.R
#'@include all-generics.R
#'@include GaApiRequest.R
#'@include GaCreds.R
NULL

# API Error response codes: https://developers.google.com/analytics/devguides/config/mgmt/v3/errors

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
        field_list[!(names(field_list) %in% c("kind", "selfLink", "childLink", "parentLink"))]
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
    id = NULL,
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
    initialize = function(parent = NULL, id = NULL, creds = GaCreds()) {
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      self$parent <- parent
      self$creds <- creds
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
        private$cache[[class_name]] <- class_generator$new(parent = self, creds = self$creds)
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
    },
    api_list = function() {
      list(
        id = self$id,
        name = self$name
      )
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
      entity <- private$entity_class$new(parent = self$parent, id = id, creds = self$creds)
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
    insert = function(entity, scope = ga_scopes["edit"]) {
      stopifnot(is(entity, private$entity_class$classname))
      entity_body_list <- entity$api_list
      ga_api_request(
        creds = self$creds,
        request = c("management", self$.req_path),
        scope = scope,
        req_type = "POST",
        body_list = entity_body_list
      )
      self$get()
      return(self)
    },
    initialize = function(parent = NULL, creds = GaCreds()) {
      entity_class_private <- with(private$entity_class, c(private_fields, private_methods))
      private$request <- entity_class_private$request
      private$parent_class_name <- entity_class_private$parent_class_name
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      self$creds <- creds
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
            entity <- private$entity_class$new(parent = self$parent, creds = self$creds)
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

gaUserSegment <- R6Class(
  "gaUserSegment",
  inherit = .gaResource,
  public = list(
    type = NA,
    segmentId = NA,
    definition = NA,
    get = function() {
      if (!is.null(self$.req_path)) {
        user_segment_fields <- subset(
          gaUserSegments$new(creds = self$creds)$summary,
          subset = id == self$id
        )
        self$modify(user_segment_fields)
      }
      self
    }
  ),
  active = list(
    api_list = function(){
      c(super$api_list, list(
        type = self$type,
        segmentId = self$segmentId,
        definition = self$definition
      ))
    }
  ),
  private = list(
    parent_class_name = "NULL",
    request = "segments",
    field_corrections = function(field_list) {
      field_list <- super$field_corrections(field_list)
      mutate(
        field_list,
        type = factor(type, levels = user_segment_type_levels)
      )
    }
  )
)

#' @export
GaUserSegment <- function(id = NULL, definition = NA, creds = GaCreds()){
  id <- sub("^gaid::", "", id)
  userSegment <- gaUserSegment$new(id = id, creds = creds)
  if (is.null(id)) {
    userSegment$definition <- definition
  }
  userSegment
}

gaUserSegments <- R6Class(
  "gaUserSegments",
  inherit = .gaCollection,
  public = list(
    insert = NULL
  ),
  private = list(
    entity_class = gaUserSegment,
    field_corrections = gaUserSegment$private_methods$field_corrections
  )
)

#' @export
GaUserSegments <- function(creds = GaCreds()){
  gaUserSegments$new(creds = creds)
}

gaAccountSummary <- R6Class(
  "gaAccountSummary",
  inherit = .gaResource,
  public = list(
    get = function() {
      if (!is.null(self$.req_path)) {
        account_summary_fields <- subset(
          gaAccountSummaries$new(creds = self$creds)$summary,
          subset = id == self$id
        )
        self$modify(account_summary_fields)
      }
      self
    }
  ),
  private = list(
    parent_class_name = "NULL",
    request = "accountSummaries"
  )
)

#' @export
GaAccountSummary <- function(id = NULL, creds = GaCreds()){
  gaAccountSummary$new(id = id, creds = creds)
}

gaAccountSummaries <- R6Class(
  "gaAccountSummaries",
  inherit = .gaCollection,
  public = list(
    insert = NULL
  ),
  private = list(
    entity_class = gaAccountSummary
  )
)

#' @export
GaAccountSummaries <- function(creds = GaCreds()){
  gaAccountSummaries$new(creds = creds)
}

gaAccount <- R6Class(
  "gaAccount",
  inherit = .gaResource,
  public = list(
    get = function() {
      if (!is.null(self$.req_path)) {
        account_fields <- subset(
          gaAccounts$new(creds = self$creds)$summary,
          subset = id == self$id
        )
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
    },
    filters = function() {
      self$.child_nodes(gaViewFilters)
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
GaAccount <- function(id = NULL, creds = GaCreds()){
  gaAccount$new(id = id, creds = creds)
}

gaAccounts <- R6Class(
  "gaAccounts",
  inherit = .gaCollection,
  public = list(
    insert = NULL
  ),
  private = list(
    entity_class = gaAccount,
    field_corrections = gaAccount$private_methods$field_corrections
  )
)

#' @export
GaAccounts <- function(creds = GaCreds()){
  gaAccounts$new(creds = creds)
}

#' @export
gaViewFilter <- R6Class(
  "gaViewFilter",
  inherit = .gaResource,
  public = list(
    type = NA,
    details = NA
  ),
  active = list(
    api_list = function() {
      type <- self$type
      details <- as.list(self$details[[1]])
      details <- switch(self$type, 
        ADVANCED = list(advancedDetails = details),
        EXCLUDE = list(excludeDetails = details),
        INCLUDE = list(includeDetails = details),
        LOWERCASE = list(lowercaseDetails = details),
        SEARCH_AND_REPLACE = list(searchAndReplaceDetails = details),
        UPPERCASE = list(uppercaseDetails = details)
      )
      c(super$api_list, list(type = type), details) 
    }
  ),
  private = list(
    parent_class_name = "gaAccount",
    request = "filters",
    field_corrections = function(field_list){
      field_list <- super$field_corrections(field_list)
      details <- alply(field_list, 1, function(row) {
        details <- switch(row$type, 
          ADVANCED = row$advancedDetails,
          EXCLUDE = row$excludeDetails,
          INCLUDE = row$includeDetails,
          LOWERCASE = row$lowercaseDetails,
          SEARCH_AND_REPLACE = row$searchAndReplaceDetails,
          UPPERCASE = row$uppercaseDetails
        )
      })
      attributes(details) <- NULL
      field_list$details <- details
      field_list
    }
  )
)

#' @export
gaViewFilters <- R6Class(
  "gaViewFilters",
  inherit = .gaCollection,
  private = list(
    entity_class = gaViewFilter,
    field_corrections = gaViewFilter$private_methods$field_corrections
  )
)

#' @export
gaAccountUserLink <- R6Class(
  "gaAccountUserLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaAccount",
    request = "entityUserLinks",
    scope = ga_scopes[c('read_only', 'manage_users')]
  )
)

#' @export
gaAccountUserLinks <- R6Class(
  "gaAccountUserLinks",
  inherit = .gaCollection,
  public = list(
    insert = function(entity) {
      super$insert(entity = entity, scope = private$scope)
    }
  ),
  private = list(
    entity_class = gaAccountUserLink,
    scope = gaAccountUserLink$private_fields$scope
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
    api_list = function() {
      c(super$api_list, list(
        websiteUrl = self$websiteUrl,
        industryVertical = self$industryVertical,
        defaultProfileId = self$defaultViewId
      ))
    },
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
        names(field_list)[names(field_list) == "defaultProfileId"] <- "defaultViewId"
      }
      field_list
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
    scope = ga_scopes[c('read_only', 'manage_users')]
  )
)

#' @export
gaPropertyUserLinks <- R6Class(
  "gaPropertyUserLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaPropertyUserLink,
    scope = gaPropertyUserLink$private_fields$scope
  )
)

#' @export
gaAdwordsLink <- R6Class(
  "gaAdwordsLink",
  inherit = .gaResource,
  public = list(
    adWordsAccounts = NA
  ),
  active = list(
    api_list = function() {
      c(super$api_list, list(
        adWordsAccounts = self$adWordsAccounts
      ))
    }
  ),
  private = list(
    parent_class_name = "gaProperty",
    request = "entityAdWordsLinks"
  )
)

#' @export
gaAdwordsLinks <- R6Class(
  "gaAdwordsLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaAdwordsLink
  )
)

#' @export
gaCustomDimension <- R6Class(
  "gaCustomDimension",
  inherit = .gaResource,
  public = list(
    index = NA,
    scope = NA,
    active = NA
  ),
  active = list(
    api_list = function() {
      c(super$api_list, list(
        index = self$index,
        scope = self$scope,
        active = self$active
      ))
    }
  ),
  private = list(
    parent_class_name = "gaProperty",
    request = "customDimensions"
  )
)

#' @export
gaCustomDimensions <- R6Class(
  "gaCustomDimensions",
  inherit = .gaCollection,
  private = list(
    entity_class = gaCustomDimension
  )
)

#' @export
gaCustomMetric <- R6Class(
  "gaCustomMetric",
  inherit = .gaResource,
  public = list(
    index = NA,
    scope = NA,
    active = NA,
    type = NA,
    min_value = NA,
    max_value = NA
  ),
  active = list(
    api_list = function() {
      c(super$api_list, list(
        index = self$index,
        scope = self$scope,
        active = self$active,
        type = self$type,
        min_value = self$min_value,
        max_value = self$max_value
      ))
    }
  ),
  private = list(
    parent_class_name = "gaProperty",
    request = "customMetrics"
  )
)

#' @export
gaCustomMetrics <- R6Class(
  "gaCustomMetrics",
  inherit = .gaCollection,
  private = list(
    entity_class = gaCustomMetric
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
    viewsLinked = NA
  ),
  active = list(
    api_list = function(){
      c(super$api_list, list(
        description = self$description,
        type = self$type,
        importBehavior = self$importBehavior,
        profilesLinked = self$viewsLinked
      ))
    },
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
  public = list(
    insert = NULL
  ),
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
    api_list = function() {
      c(super$api_list, list(
        type = self$type,
        currency = self$currency,
        timezone = self$timezone,
        websiteUrl = self$websiteUrl,
        defaultPage = self$defaultPage,
        excludeQueryParameters = self$excludeQueryParameters,
        siteSearchQueryParameters = self$siteSearchQueryParameters,
        stripSiteSearchQueryParameters = self$stripSiteSearchQueryParameters,
        siteSearchCategoryParameters = self$siteSearchCategoryParameters,
        stripSiteSearchCategoryParameters = self$stripSiteSearchCategoryParameters,
        eCommerceTracking = self$eCommerceTracking,
        enhancedECommerceTracking = self$enhancedECommerceTracking
      ))
    },
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
      field_list$type = factor(field_list$type, levels = view_type_levels)
      field_list$currency = factor(field_list$currency, levels = currency_levels)
      field_list$stripSiteSearchQueryParameters = identical(field_list$stripSiteSearchQueryParameters, TRUE)
      field_list$stripSiteSearchCategoryParameters = identical(field_list$stripSiteSearchCategoryParameters, TRUE)
      field_list
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
    api_list = function(){
      c(list(
        type = self$type,
        value = self$value,
        active = self$active,
        details = self$details
      ))
    },
    parent_class_name = "gaView",
    request = "goals"
  )
)

#' @export
gaGoals <- R6Class(
  "gaGoals",
  inherit = .gaCollection,
  private = list(
    entity_class = gaGoal
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
  active = list(
    api_list = function(){
      c(super$api_list, list(
        description = self$description,
        objectiveMetric = self$objectiveMetric,
        optimizationType = self$optimizationType,
        status = self$status,
        winnerFound = self$winnerFound,
        startTime = self$startTime,
        endTime = self$endTime,
        reasonExperimentEnded = self$reasonExperimentEnded,
        rewriteVariationUrlsAsOriginal = self$rewriteVariationUrlsAsOriginal,
        winnerConfidenceLevel = self$winnerConfidenceLevel,
        minimumExperimentLengthInDays = self$minimumExperimentLengthInDays,
        trafficCoverage = self$trafficCoverage,
        equalWeighting = self$equalWeighting,
        servingFramework = self$servingFramework,
        variations = self$variations
      ))
    }
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
    scope = ga_scopes[c('read_only', 'manage_users')]
  )
)

#' @export
gaViewUserLinks <- R6Class(
  "gaViewUserLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaViewUserLink,
    scope = gaViewUserLink$private_fields$scope
  )
)

#' @export
gaViewFilterLink <- R6Class(
  "gaViewFilterLink",
  inherit = .gaResource,
  private = list(
    parent_class_name = "gaView",
    request = "profileFilterLinks"
  )
)

#' @export
gaViewFilterLinks <- R6Class(
  "gaViewFilterLinks",
  inherit = .gaCollection,
  private = list(
    entity_class = gaViewFilterLink
  )
)

