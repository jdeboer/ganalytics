#' @include GaApiRequest.R

gtm_api_request <- function(
  creds,
  request,
  scope = gtm_scopes["read_only"],
  base_url = "https://www.googleapis.com/tagmanager/v1",
  req_type = "GET",
  body_list = NULL,
  fields = NULL,
  max_results = NULL
) {
  stopifnot(scope %in% gtm_scopes)
  google_api_request(
    creds = creds,
    scope = scope,
    request = request,
    base_url = base_url,
    req_type = req_type,
    body_list = body_list,
    fields = fields
  )
}

.gtmManagementApi <- R6Class(
  ".gtmManagementApi",
  inherit = .googleApi,
  private = list(
    scope = gtm_scopes['read_only'],
    write_scope = gtm_scopes["edit_containers"],
    api_req_func = gtm_api_request,
    base_url = "https://www.googleapis.com/tagmanager/v1"
  )
)

.gtmResource <- R6Class(
  ".gtmResource",
  inherit = .googleApiResource,
  private = c(
    get_privates(.gtmManagementApi),
    list(field_corrections = function(field_list) {
      names(field_list)[names(field_list) == paste0(private$resource_name, 'Id')] <- "id"
      super$field_corrections(field_list)
    })
  )
)

.gtmCollection <- R6Class(
  ".gtmCollection",
  inherit = .googleApiCollection,
  private = c(
    get_privates(.gtmResource),
    list(collection_name = NULL)
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

#' GTM Account
#'
#' Get a GTM account.
#'
#' @param id ID of the GTM account to get
#' @param creds The Google APIs credentials to use.
#'
#' @export
GtmAccount <- function(id = NULL, creds = .creds){
  gtmAccount$new(id = id, creds = creds)
}

gtmAccounts <- R6Class(
  "gtmAccounts",
  inherit = .gtmCollection,
  private = list(
    entity_class = gtmAccount
  )
)

#' GTM Accounts
#'
#' Get a collection of GTM accounts.
#'
#' @param creds The Google APIs credentials to use.
#'
#' @export
GtmAccounts <- function(creds = .creds){
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

