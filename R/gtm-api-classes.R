#'@include ganalytics-package.R
#'@include all-generics.R
#'@include GaApiRequest.R
#'@include GaCreds.R
NULL

.gtmManagementApi <- R6Class(
  ".gtmManagementApi",
  public = list(
    creds = GaCreds(),
    get = function() {
      gtm_api_request(
        creds = self$creds,
        request = self$.req_path,
        scope = private$scope
      )
    }
  ),
  private = list(
    request = NULL,
    scope = gtm_scopes['read_only'],
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
    parent_class_name = "NULL",
    resource_name = NULL
  )
)

.gtmResource <- R6Class(
  ".gtmResource",
  inherit = .gtmManagementApi,
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
    }
  ),
  private = list(
    cache = list()
  )
)

.gtmCollection <- R6Class(
  ".gtmCollection",
  inherit = .gtmManagementApi,
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
        self$summary <- private$field_corrections(response[[private$collection_name]])
      }
      self
    },
    initialize = function(parent = NULL, creds = GaCreds()) {
      entity_class_private <- with(private$entity_class, c(private_fields, private_methods))
      private$request <- entity_class_private$request
      private$parent_class_name <- entity_class_private$parent_class_name
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      if(is.null(private$collection_name)) {
        private$collection_name <- private$request
      }
      private$resource_name <- entity_class_private$resource_name
      self$creds <- creds
      self$parent <- parent
      self$get()
      self
    }
  ),
  active = list(
    entities = function() {
      if (is.data.frame(self$summary)) {
        ret <- alply(self$summary, 1, function(summary_row) {
          field_list <- as.list(summary_row)
          names(field_list)[names(field_list) == paste0(private$resource_name, 'Id')] <- "id"
          id <- field_list$id
          entity <- private$entities_cache[[id]]
          if (!is(entity, private$entity_class$classname)) {
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
    entity_class = .gtmResource,
    entities_cache = list(),
    collection_name = NULL
  )
)

gtmAccount <- R6Class(
  "gtmAccount",
  inherit = .gtmResource,
  public = list(
    shareData = NA,
    fingerprint = NA
  ),
  active = list(
    containers = function() {self$.child_nodes(gtmContainers)},
    permissions = function() {
      tryCatch(
        self$.child_nodes(gtmPermissions),
        error = function(e) {
          e$message
        }
      )
    }
  ),
  private = list(
    parent_class_name = "NULL",
    request = "accounts",
    resource_name = "account"
  )
)

#' @export
GtmAccount <- function(id = NULL, creds = GaCreds()){
  gtmAccount$new(id = id, creds = creds)
}

gtmAccounts <- R6Class(
  "gtmAccounts",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmAccount
  )
)

#' @export
GtmAccounts <- function(creds = GaCreds()){
  gtmAccounts$new(creds = creds)
}

gtmPermission <- R6Class(
  "gtmPermission",
  inherit = .gtmResource,
  public = list(
    emailAddress = NA,
    accountAccess = NA,
    containerAccess = NA
  ),
  private = list(
    parent_class_name = "gtmAccount",
    request = "permissions",
    scope = gtm_scopes[c('read_only', 'manage_users')],
    resource_name = "permission"
  )
)

gtmPermissions <- R6Class(
  "gtmPermissions",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmPermission,
    collection_name = "userAccess",
    scope = gtmPermission$private_fields$scope
  )
)

gtmContainer <- R6Class(
  "gtmContainer",
  inherit = .gtmResource,
  public = list(
    domainName = NA,
    publicId = NA,
    timeZoneCountryId = NA,
    timeZoneId = NA,
    notes = NA,
    usageContext = NA,
    enabledBuiltInVariable = NA,
    fingerprint = NA
  ),
  active = list(
    tags = function() {self$.child_nodes(gtmTags)},
    rules = function() {self$.child_nodes(gtmRules)},
    macros = function() {self$.child_nodes(gtmMacros)},
    versions = function() {self$.child_nodes(gtmContainerVersions)},
    variables = function() {self$.child_nodes(gtmVariables)},
    triggers = function() {
      tryCatch(
        self$.child_nodes(gtmTriggers),
        error = function(e) {
          e$message
        }
      )
    }
  ),
  private = list(
    parent_class_name = "gtmAccount",
    request = "containers",
    resource_name = "container"
  )
)

gtmContainers <- R6Class(
  "gtmContainers",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmContainer
  )
)

gtmTag <- R6Class(
  "gtmTag",
  inherit = .gtmResource,
  public = list(
    type = NA,
    firingRuleId = NA,
    blockingRuleId = NA,
    firingTriggerId = NA,
    blockingTriggerId = NA,
    liveOnly = NA,
    priority = NA,
    notes = NA,
    scheduleStartMs = NA,
    scheduleEndMs = NA,
    parameter = NA,
    fingerprint = NA
  ),
  private = list(
    parent_class_name = "gtmContainer",
    request = "tags",
    resource_name = "tag"
  )
)

gtmTags <- R6Class(
  "gtmTags",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmTag
  )
)

gtmRule <- R6Class(
  "gtmRule",
  inherit = .gtmResource,
  public = list(
    notes = NA,
    condition = NA,
    fingerprint = NA
  ),
  private = list(
    parent_class_name = "gtmContainer",
    request = "rules",
    resource_name = "rule"
  )
)

gtmRules <- R6Class(
  "gtmRules",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmRule
  )
)

gtmTrigger <- R6Class(
  "gtmTrigger",
  inherit = .gtmResource,
  public = list(
    type = NA,
    customEventFilter = NA,
    filter = NA,
    autoEventFilter = NA,
    waitForTags = NA,
    checkValidation = NA,
    waitForTagsTimeout = NA,
    uniqueTriggerId = NA,
    eventName = NA,
    interval = NA,
    limit = NA,
    enableAllVideos = NA,
    videoPercentageList = NA,
    fingerprint = NA
  ),
  private = list(
    parent_class_name = "gtmContainer",
    request = "triggers",
    resource_name = "trigger"
  )
)

gtmTriggers <- R6Class(
  "gtmTriggers",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmTrigger
  )
)

gtmMacro <- R6Class(
  "gtmMacro",
  public = list(
    type = NA,
    notes = NA,
    scheduleStartMs = NA,
    scheduleEndMs = NA,
    parameter = NA,
    enablingRuleId = NA,
    disablingRuleId = NA,
    fingerprint = NA
  ),
  inherit = .gtmResource,
  private = list(
    parent_class_name = "gtmContainer",
    request = "macros",
    resource_name = "macro"
  )
)

gtmMacros <- R6Class(
  "gtmMacros",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmMacro
  )
)

gtmVariable <- R6Class(
  "gtmVariable",
  inherit = .gtmResource,
  public = list(
    type = NA,
    notes = NA,
    scheduleStartMs = NA,
    scheduleEndMs = NA,
    parameter = NA,
    enablingTriggerId = NA,
    disablingTriggerId = NA,
    fingerprint = NA
  ),
  private = list(
    parent_class_name = "gtmContainer",
    request = "variables",
    resource_name = "variable"
  )
)

gtmVariables <- R6Class(
  "gtmVariables",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmVariable
  )
)

gtmContainerVersion <- R6Class(
  "gtmContainerVersion",
  inherit = .gtmResource,
  public = list(
    deleted = NA,
    notes = NA,
    container = NA,
    macro = NA,
    rule = NA,
    tag = NA,
    trigger = NA,
    variable = NA,
    fingerprint = NA
  ),
  private = list(
    parent_class_name = "gtmContainer",
    request = "versions",
    resource_name = "containerVersion"
  )
)

gtmContainerVersions <- R6Class(
  "gtmContainerVersions",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmContainerVersion,
    collection_name = "containerVersion"
  )
)

