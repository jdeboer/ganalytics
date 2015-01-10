view_type_levels <- c("WEB", "APP")

currency_levels <- c(
  "ARS", "AUD", "BGN", "BRL", "CAD", "CHF",
  "CNY", "CZK", "DKK", "EUR", "GBP", "HKD",
  "HUF", "IDR", "INR", "JPY", "KRW", "LTL",
  "MXN", "NOK", "NZD", "PHP", "PLN", "RUB",
  "SEK", "THB", "TRY", "TWD", "USD", "VND", "ZAR"
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
    id = NULL,
    name = NULL,
    created = NULL,
    updated = NULL,
    parent = NULL,
    modify = function(field_list) {
      if(!is.null(field_list$id)) self$id <- field_list$id
      if(!is.null(field_list$name)) self$name <- field_list$name
      if(!is.null(field_list$created)) self$created <- field_list$created
      if(!is.null(field_list$updated)) self$updated <- field_list$updated
      self
    },
    initialize = function(parent = NULL, id = NULL) {
      self$parent <- parent
      self$id <- id
      self$get()
    },
    refresh = function() {
      self$initialize(id = self$id)
    },
    get = function() {
      if (!is.null(self$id)) {
        scope <- ga_scopes['read_only']
        response <- ga_api_request(
          creds = self$creds,
          request = c("management", self$req_path),
          scope = scope
        )
        field_list <- as.list(mutate(
          response,
          created = ymd_hms(created),
          updated = ymd_hms(updated)
        ))
        self$modify(field_list)
        private$response <- response
      }
      return(self)
    }
  ),
  active = list(
    req_path = function() {
      if (is.null(self$id)) {
        return(NULL)
      } else {
        c(self$parent$req_path, private$request, self$id)
      }
    }
  ),
  private = list(
    request = character(),
    response = list()
  )
)

.GaCollection <- R6Class(
  ".GaCollection",
  inherit = .GaManagementApi,
  public = list(
    summary = data.frame(),
    parent = NULL,
    get_entity = function(id) {
      private$entity_class$new(parent = self$parent, id = id)
    },
    get = function() {
      if (!is.null(self$req_path)) {
        scope <- ga_scopes['read_only']
        response <- ga_api_request(
          creds = self$creds,
          request = c("management", self$req_path),
          scope = scope
        )
        response$items <- mutate(
          response$items,
          created = ymd_hms(created),
          updated = ymd_hms(updated)
        )
        self$summary <- response$items
        rownames(self$summary) <- self$summary$id
      }
      return(self)
    },
    initialize = function(parent = NULL) {
      self$parent = parent
      self$get()
    },
    refresh = function() {
      self$initialize(parent = self$parent)
    }
  ),
  active = list(
    #     entities = function() {
    #       ret <- llply(self$summary$id, function(id) {
    #         self$get_entity(id = id)
    #       })
    #       names(ret) <- self$summary$id
    #       return(ret)
    #     },
    entities = function() {
      ret <- alply(self$summary, 1, function(summary_row) {
        field_list <- as.list(summary_row)
        entity <- private$entity_class$new(parent = self$parent)
        entity$modify(field_list = field_list)
        entity
      })
      attributes(ret) <- NULL
      names(ret) <- self$summary$id
      ret
    },
    req_path = function() {
      if (!is.null(self$parent) & is.null(self$parent$req_path)) {
        return(NULL)
      } else {
        c(self$parent$req_path, private$request)
      }
    }
  ),
  private = list(
    request = character(),
    entity_class = NULL
  )
)

#' @export
GaAccount <- R6Class(
  "GaAccount",
  inherit = .GaResource,
  public = list(
    get = function() {
      if (!is.null(self$id)) {
        account <- GaAccounts$new()$summary[self$id, ]
        self$modify(as.list(account))
      }
      self
    },
    initialize = function(parent = NULL, id = NULL) {
      stopifnot(is(parent, "NULL"))
      super$initialize(parent = parent, id = id)
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
    request = "accounts",
    properties_cache = NULL
  )
)

#' @export
GaAccounts <- R6Class(
  "GaAccounts",
  inherit = .GaCollection,
  public = list(
    initialize = function(parent = NULL) {
      stopifnot(is(parent, "NULL"))
      super$initialize(parent)
    }
  ),
  private = list(
    request = "accounts",
    entity_class = GaAccount
  ),
  active = list(
    entities = function() {
      ret <- alply(self$summary, 1, function(summary_row) {
        field_list <- as.list(summary_row)
        entity <- private$entity_class$new(parent = self$parent)
        entity$modify(field_list = field_list)
        entity
      })
      attributes(ret) <- NULL
      names(ret) <- self$summary$id
      ret
    }
  )
)

#' @export
GaProperty <- R6Class(
  "GaProperty",
  inherit = .GaResource,
  public = list(
    websiteUrl = NULL,
    industryVertical = NULL,
    defaultViewId = NULL,
    modify = function(field_list) {
      super$modify(field_list)
      if(!is.null(field_list$websiteUrl)) self$websiteUrl <- field_list$websiteUrl
      if(!is.null(field_list$industryVertical)) self$industryVertical <- field_list$industryVertical
      if(!is.null(field_list$defaultViewId)) self$defaultViewId <- field_list$defaultProfileId
      self
    },
    get = function() {
      super$get()
      property <- private$response
      self$modify(as.list(property))
      self
    },
    initialize = function(parent, id = NULL) {
      stopifnot(is(parent, "GaAccount"))
      super$initialize(parent, id)
    }
  ),
  active = list(
    views = function() {
      if (is(private$views_cache, "GaViews")) {
        private$views_cache
      } else {
        private$views_cache <- GaViews$new(parent = self)
      } 
    }
  ),
  private = list(
    request = "webproperties",
    views_cache = NULL
  )
)

#' @export
GaProperties <- R6Class(
  "GaProperties",
  inherit = .GaCollection,
  public = list(
    initialize = function(parent) {
      stopifnot(is(parent, "GaAccount"))
      super$initialize(parent)
    }
  ),
  private = list(
    request = "webproperties",
    entity_class = GaProperty
  )
)

#' @export
GaView <- R6Class(
  "GaView",
  inherit = .GaResource,
  public = list(
    type = NULL,
    currency = NULL,
    timezone = NULL,
    websiteUrl = NULL,
    defaultPage = NULL,
    excludeQueryParameters = NULL,
    siteSearchQueryParameters = NULL,
    stripSiteSearchQueryParameters = NULL,
    siteSearchCategoryParameters = NULL,
    stripSiteSearchCategoryParameters = NULL,
    eCommerceTracking = NULL,
    enhancedECommerceTracking = NULL,
    modify = function(field_list) {
      super$modify(field_list)
      if(!is.null(field_list$type)) self$type <- factor(field_list$type, levels = view_type_levels)
      if(!is.null(field_list$currency)) self$currency <- factor(field_list$currency, levels = currency_levels)
      if(!is.null(field_list$timezone)) self$timezone <- as.character(field_list$timezone)
      if(!is.null(field_list$websiteUrl)) self$websiteUrl <- as.character(field_list$websiteUrl)
      if(!is.null(field_list$defaultPage)) self$defaultPage <- as.character(field_list$defaultPage)
      if(!is.null(field_list$excludeQueryParameters)) self$excludeQueryParameters <- as.character(field_list$excludeQueryParameters)
      if(!is.null(field_list$siteSearchQueryParameters)) self$siteSearchQueryParameters <- as.character(field_list$siteSearchQueryParameters)
      if(!is.null(field_list$stripSiteSearchQueryParameters)) self$stripSiteSearchQueryParameters <- identical(field_list$stripSiteSearchQueryParameters, TRUE)
      if(!is.null(field_list$siteSearchCategoryParameters)) self$siteSearchCategoryParameters <- as.character(field_list$siteSearchCategoryParameters)
      if(!is.null(field_list$stripSiteSearchCategoryParameters)) self$stripSiteSearchCategoryParameters <- identical(field_list$stripSiteSearchCategoryParameters, TRUE)
      if(!is.null(field_list$eCommerceTracking)) self$eCommerceTracking <- field_list$eCommerceTracking
      if(!is.null(field_list$enhancedECommerceTracking)) self$enhancedECommerceTracking <- field_list$enhancedECommerceTracking
      self
    },
    get = function() {
      super$get()
      view <- private$response
      self$modify(as.list(view))
      self
    },
    initialize = function(parent, id = NULL) {
      stopifnot(is(parent, "GaProperty"))
      super$initialize(parent, id)
    }
  ),
  private = list(
    request = "profiles"
  )
)

#' @export
GaViews <- R6Class(
  "GaViews",
  inherit = .GaCollection,
  public = list(
    get = function() {
      super$get()
      self$summary$type <- factor(self$summary$type, levels = view_type_levels)
      self$summary$currency <- factor(self$summary$currency, levels = currency_levels)
      self$summary$stripSiteSearchQueryParameters <- identical(self$summary$stripSiteSearchQueryParameters, TRUE)
      self$summary$stripSiteSearchCategoryParameters <- identical(self$summary$stripSiteSearchCategoryParameters, TRUE)
    },
    initialize = function(parent) {
      stopifnot(is(parent, "GaProperty"))
      super$initialize(parent)
    }
  ),
  private = list(
    request = "profiles",
    entity_class = GaView
  )
)