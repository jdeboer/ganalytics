#' @include GaApiRequest.R
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd_hms
#' @importFrom plyr l_ply alply
#' @importFrom utils URLencode

get_privates <- function(class_gen){
  with(class_gen, c(private_fields, private_methods))
}

.googleApi <- R6Class(
  ".googleApi",
  public = list(
    creds = get_creds(),
    get = function(max_results = NULL) {
      req_type <- "GET"
      private$api_req_func(
        creds = self$creds,
        request = self$.req_path,
        scope = private$scope,
        base_url = private$base_url,
        req_type = req_type,
        max_results = max_results
      )
    },
    print = function(...) {
      cat("<", class(self)[1], ">\n")
      # cat("  $creds\n")
      # cat("    appname : ", self$creds$app$appname, "\n")
      # cat("    key     : ", self$creds$app$key, "\n")
      # cat("    login   : ", self$creds$user$login, "\n")
      # cat("    cache   : ", self$creds$user$cache, "\n")
      # cat("  $get()\n")
    },
    initialize = function(creds = get_creds()) {
      self$creds = creds
    }
  ),
  active = list(
    .req_path = function(){}
  ),
  private = list(
    request = NULL,
    scope = NULL,
    parent_class_name = "NULL",
    resource_name = NULL,
    base_url = NULL,
    api_req_func = google_api_request,
    field_corrections = function(field_list) {
      if (is.data.frame(field_list)) {
        if (exists("created", field_list)) {
          field_list$created <- ymd_hms(field_list$created)
        }
        if (exists("updated", field_list)) {
          field_list$updated <- ymd_hms(field_list$updated)
        }
      }
      field_list[!(names(field_list) %in% c("kind", "selfLink", "childLink", "parentLink"))]
    }
  )
)

.googleApiResource <- R6Class(
  ".googleApiResource",
  inherit = .googleApi,
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
    print = function(...) {
      super$print(...)
      cat("  $id       = ", self$id, "\n")
      cat("  $name     = ", self$name, "\n")
      cat("  $created  = ", self$created, "\n")
      cat("  $updated  = ", self$updated, "\n")
      cat("  $parent   : <", class(self$parent)[1], ">\n")
      if (!is.null(self$UPDATE)) cat("  $UPDATE()\n")
    },
    initialize = function(creds = get_creds(), parent = NULL, id = NA) {
      super$initialize(creds = creds)
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      self$parent <- parent
      self$id <- id
      if (!is.na(id)) {
        self$get()
      }
      self
    },
    get = function() {
      if (!is.null(self$.req_path)) {
        response <- super$get()
        updated_fields <- private$field_corrections(response)
        self$modify(updated_fields)
      }
      self
    },
    UPDATE = function(scope = private$write_scope) {
      entity_body_list <- self$api_list
      private$api_req_func(
        creds = self$creds,
        request = self$.req_path,
        scope = scope,
        base_url = private$base_url,
        req_type = "PUT",
        body_list = entity_body_list
      )
      self$get()
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
      if (is.na(self$id)) {
        NULL
      } else {
        c(self$parent$.req_path, private$request, URLencode(self$id, reserved = TRUE))
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

.googleApiCollection <- R6Class(
  ".googleApiCollection",
  inherit = .googleApi,
  public = list(
    summary = data.frame(),
    parent = NULL,
    get_entity = function(id) {
      stopifnot(id %in% self$summary$id)
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
    INSERT = function(entity, scope = private$write_scope) {
      stopifnot(is(entity, private$entity_class$classname))
      entity_body_list <- entity$api_list
      private$api_req_func(
        creds = self$creds,
        request = self$.req_path,
        scope = scope,
        base_url = private$base_url,
        req_type = "POST",
        body_list = entity_body_list
      )
      self$get()
    },
    DELETE = function(id, scope = private$write_scope) {
      entity <- self$get_entity(id)
      private$api_req_func(
        creds = entity$creds,
        request = entity$.req_path,
        scope = scope,
        base_url = private$base_url,
        req_type = "DELETE"
      )
      self$get()
    },
    print = function(...) {
      super$print(...)
      cat("  $summary  : <",
          class(self$summary),"> (",
          paste(dim(self$summary), collapse = " x "), ")\n"
      )
      cat("  $parent   : <", class(self$parent)[1], ">\n")
      cat("  $entities : <",
          class(self$entities), "> (",
          length(self$entities), ")\n")
      cat("  $get_entity(id)\n")
      if (!is.null(self$INSERT)) cat("  $INSERT(entity)\n")
      if (!is.null(self$DELETE)) cat("  $DELETE(id)\n")
    },
    initialize = function(creds = get_creds(), parent = NULL) {
      super$initialize(creds = creds)
      entity_class_private <- get_privates(private$entity_class)
      private$request <- entity_class_private$request
      private$parent_class_name <- entity_class_private$parent_class_name
      stopifnot(is(parent, private$parent_class_name) | is.null(parent))
      if (is.null(private$collection_name)) {
        private$collection_name <- private$request
      }
      private$resource_name <- entity_class_private$resource_name
      self$parent <- parent
      self$get()
    }
  ),
  active = list(
    entities = function() {
      if (is.data.frame(self$summary)) {
        ret <- alply(self$summary, 1, function(summary_row) {
          field_list <- as.list(summary_row)
          updated <- summary_row$updated
          id <- field_list$id
          entity <- private$entities_cache[[id]]
          if (
            !is(entity, private$entity_class$classname) |
            !identical(entity$updated, updated)
          ) {
            entity <- private$entity_class$new(parent = self$parent, creds = self$creds)
            entity$modify(field_list = field_list)
            private$entities_cache[[id]] <- entity
          }
          entity
        })
        attributes(ret) <- NULL
        names(ret) <- self$summary$id
        ret
      } else {
        NULL
      }
    },
    .req_path = function() {
      # if this is top most level, e.g. 'Accounts' or "UserSegments", then there
      # is no parent and therefore there will not exist a parent request path,
      # i.e. it will be NULL. Otherwise, if there is a parent, but it has no
      # request path, then this should also not have a request path.
      if (!is.null(self$parent) & is.null(self$parent$.req_path)) {
        NULL
      } else if (is(self$parent, private$parent_class_name)) {
        c(self$parent$.req_path, private$request)
      } else {
        NULL
      }
    }
  ),
  private = list(
    entity_class = .googleApiResource,
    entities_cache = list(),
    collection_name = "items"
  )
)
