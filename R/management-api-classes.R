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
    request = NULL,
    field_corrections = function(field_list) {
      mutate(
        field_list,
        created = ymd_hms(created),
        updated = ymd_hms(updated)
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
        self$summary <- private$field_corrections(response$items)
        rownames(self$summary) <- self$summary$id
      }
      self
    },
    initialize = function(parent = NULL) {
      self$parent = parent
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
      # if this is top most level, e.g. 'Accounts', then there is no parent and
      # therefore there will not exist a parent request path, i.e. it will be NULL.
      # Otherwise, if there is a parent, but it has no request path, then this
      # should also not have a request path.
      if (!is.null(self$parent) & is.null(self$parent$req_path)) {
        return(NULL)
      } else {
        c(self$parent$req_path, private$request)
      }
    }
  ),
  private = list(
    request = character(),
    entity_class = .GaResource,
    field_corrections = .GaResource$private_methods$field_corrections,
    entities_cache = list()
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
    },
    initialize = function(parent = NULL, id = NULL) {
      stopifnot(parent == NULL)
      super$initialize(id = id)
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
  private = list(
    request = "accounts",
    entity_class = GaAccount
  )
)

#' @export
GaProperty <- R6Class(
  "GaProperty",
  inherit = .GaResource,
  public = list(
    websiteUrl = NA,
    industryVertical = NA,
    defaultViewId = NA,
    initialize = function(parent, id = NULL) {
      stopifnot(is(parent, "GaAccount"))
      super$initialize(parent = parent, id = id)
    }
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
      #self$views$entities[[self$defaultViewId]]
      self$views$get_entity(id = self$defaultViewId)
    }
  ),
  private = list(
    request = "webproperties",
    views_cache = NULL,
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
  public = list(
    initialize = function(parent) {
      stopifnot(is(parent, "GaAccount"))
      super$initialize(parent = parent)
    }
  ),
  private = list(
    request = "webproperties",
    entity_class = GaProperty,
    field_corrections = GaProperty$private_methods$field_corrections
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
    enhancedECommerceTracking = NA,
    modify = function(field_list) {
      field_list <- private$field_corrections(field_list)
      super$modify(field_list)
      self
    },
    initialize = function(parent, id = NULL) {
      stopifnot(is(parent, "GaProperty"))
      super$initialize(parent, id)
    }
  ),
  private = list(
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
GaViews <- R6Class(
  "GaViews",
  inherit = .GaCollection,
  public = list(
    initialize = function(parent) {
      stopifnot(is(parent, "GaProperty"))
      super$initialize(parent)
    }
  ),
  private = list(
    request = "profiles",
    entity_class = GaView,
    field_corrections = GaView$private_methods$field_corrections
  )
)

