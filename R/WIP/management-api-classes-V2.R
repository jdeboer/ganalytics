library(lubridate)
library(plyr)
library(assertthat)
library(jsonlite)
library(httr)
library(stringr)
ga_scopes <- c(
  default = "https://www.googleapis.com/auth/analytics",
  edit = "https://www.googleapis.com/auth/analytics.edit",
  read_only = "https://www.googleapis.com/auth/analytics.readonly",
  manage_users = "https://www.googleapis.com/auth/analytics.manage.users",
  read_only_users = "https://www.googleapis.com/auth/analytics.manage.users.readonly"
)
user_segment_type_levels <- c(
  "BUILT_IN", "CUSTOM"
)
source("R/GaApiRequest.R")

.googleApi <- setRefClass(
  ".googleApi",
  fields = list(
    creds = "list",
    request = "character",
    scope = "character",
    write_scope = "character",
    parent_class_name = "character",
    resource_name = "character",
    base_url = "character"
  ),
  methods = list(
    initialize = function(creds = get_creds()) {
      creds <<- creds
      request <<- character(0)
      scope <<- character(0)
      write_scope <<- character(0)
      parent_class_name <<- "NULL"
      resource_name <<- character(0)
      base_url <<- character(0)
    },
    req_path = function(){
      request
    },
    get = function(max_results = NULL) {
      "GET - gets / refreshes the details of this resource or"
      "collection from Google Analytics."
      .self$api_req_func(
        creds = creds,
        request = req_path,
        scope = scope,
        base_url = base_url,
        req_type = "GET",
        max_results = max_results
      )
    },
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
      field_list[!(names(field_list) %in% c(
        "kind", "selfLink", "childLink", "parent_entityLink"
      ))]
    }
  ),
  validity = function(object) {
    with(object, {
      validate_that(
        length(request) <= 1,
        length(scope) <= 1,
        length(write_scope) <= 1,
        length(parent_class_name) <= 1,
        length(resource_name) <= 1,
        length(base_url) <= 1
      )
    })
  }
)

.googleApiResource <- setRefClass(
  ".googleApiResource",
  fields = list(
    id = "character",
    name = "character",
    created = "POSIXct",
    updated = "POSIXct",
    parent_entity = "ANY",
    from_collection = "ANY",
    cache = "list"
  ),
  contains = ".googleApi",
  methods = list(
    initialize = function(creds = get_creds(),
                          parent_entity = NULL,
                          from_collection = .googleApiCollection$new(),
                          id = character(0)) {
      callSuper(creds = creds)
      id <<- id
      name <<- character(0)
      created <<- as.POSIXct(NA)
      updated <<- as.POSIXct(NA)
      parent_entity <<- parent_entity
      from_collection <<- from_collection
      request <<- from_collection$field("request")
      cache <<- list()
      stopifnot(
        is(parent_entity, parent_class_name) | is.null(parent_entity)
      )
      if (!is.na(id)) {
        .self$get()
      }
      .self
    },
    modify = function(field_list) {
      l_ply(names(field_list), function(field_name) {
        if (exists(field_name, .self)) {
          .self$field(field_name, field_list[[field_name]])
        }
      })
      .self
    },
    get = function() {
      if (!is.null(.self$req_path())) {
        response <- callSuper()
        updated_fields <- .self$field_corrections(response)
        .self$modify(updated_fields)
      }
      .self
    },
    UPDATE = function(scope = write_scope) {
      entity_body_list <- .self$api_list()
      .self$api_req_func(
        creds = creds,
        request = .self$req_path(),
        scope = scope,
        base_url = base_url,
        req_type = "PUT",
        body_list = entity_body_list
      )
      .self$get()
    },
    child_nodes = function(class_generator) {
      class_name <- class_generator$classname
      if (is(cache[[class_name]], class_name)) {
        cache[[class_name]]
      } else {
        cache[[class_name]] <<- class_generator$new(
          parent_entity = .self, creds = creds
        )
      }
    },
    req_path = function() {
      if (is.na(id)) {
        NULL
      } else {
        c(callSuper(), request, URLencode(id, reserved = TRUE))
      }
    },
    api_list = function() {
      list(
        id = id,
        name = name
      )
    }
  ),
  validity = function(object) {
    with(object, {
      validate_that(
        length(id) <= 1,
        length(name) <= 1,
        length(created) <= 1,
        length(updated) <= 1
      )
    })
  }
)

.googleApiCollection <- setRefClass(
  ".googleApiCollection",
  fields = list(
    summary = "data.frame",
    parent_entity = "ANY",
    entity_class_generator = "refObjectGenerator",
    entities_cache = "list",
    collection_name = "character"
  ),
  contains = ".googleApi",
  methods = list(
    initialize = function(creds = get_creds(), parent_entity = NULL) {
      callSuper(creds = creds)
      summary <<- data.frame()
      parent_entity <<- NULL
      entity_class_generator <<- .googleApiResource
      entities_cache <<- list()
      collection_name <<- "items"
      stopifnot(
        is(parent_entity, parent_class_name) | is.null(parent_entity)
      )
      if (length(collection_name) == 0) {
        collection_name <<- request
      }
      if (is.null(parent_entity)) {
        resource_name <<- character(0)
      } else {
        resource_name <<- parent_entity$resource_name
      }
      parent_entity <<- parent_entity
      .self$get()
    },
    get_entity = function(id) {
      stopifnot(id %in% summary$id)
      entity <- entity_class_generator$new(
        parent_entity = parent_entity,
        id = id,
        creds = creds
      )
      entities_cache[[id]] <<- entity
      entity
    },
    get = function() {
      if (!is.null(.self$req_path())) {
        summary <<- .self$field_corrections(callSuper()[[collection_name]])
      }
      .self
    },
    INSERT = function(entity, scope = write_scope) {
      stopifnot(is(entity, entity_class_generator$className))
      entity_body_list <- entity$api_list
      .self$api_req_func(
        creds = creds,
        request = req_path,
        scope = scope,
        base_url = base_url,
        req_type = "POST",
        body_list = entity_body_list
      )
      .self$get()
    },
    DELETE = function(id, scope = write_scope) {
      entity <- .self$get_entity(id)
      .self$api_req_func(
        creds = entity$creds,
        request = entity$req_path,
        scope = scope,
        base_url = base_url,
        req_type = "DELETE"
      )
      .self$get()
    },
    entities = function() {
      if (is.data.frame(summary)) {
        ret <- alply(summary, 1, function(summary_row) {
          field_list <- as.list(summary_row)
          new_updated <- summary_row$updated
          new_id <- field_list$id
          entity <- entities_cache[[new_id]]
          if (
            !is(entity, entity_class_generator$className) |
            !identical(entity$updated, new_updated)
          ) {
            entity <- entity_class_generator$new(
              parent_entity = .self$parent_entity,
              creds = creds
            )
            entity$modify(field_list = field_list)
            entities_cache[[new_id]] <<- entity
          }
          entity
        })
        attributes(ret) <- NULL
        names(ret) <- summary$id
        ret
      } else {
        NULL
      }
    },
    req_path = function() {
      # if this is top most level, e.g. 'Accounts' or "UserSegments", then there
      # is no parent_entity and therefore there will not exist a parent_entity
      # request path, i.e. it will be NULL. Otherwise, if there is a
      # parent_entity, but it has no request path, then this should also not
      # have a request path.
      if (!is.null(parent_entity)) {
        if (is.null(parent_entity$req_path())) {
          return(NULL)
        }
      }
      if (is(parent_entity, parent_class_name)) {
        if (is.null(parent_entity)) {
          return(NULL)
        } else {
          c(parent_entity$req_path(), request)
        }
      } else {
        NULL
      }
    }
  )
)

.gaManagementApi <- setRefClass(
  ".gaManagementApi",
  contains = ".googleApi",
  methods = list(
    initialize = function() {
      callSuper()
      scope <<- ga_scopes['read_only']
      write_scope <<- ga_scopes["edit"]
      base_url <<- "https://www.googleapis.com/analytics/v3"
    },
    api_req_func = function(request, ...){
      req <- c(
        "management",
        "upload"[attr(request, "upload")],
        request
      )
      callSuper(request = req, ...)
    }
  )
)

gaResource <- setRefClass(
  "gaResource",
  contains = c(".googleApiResource", ".gaManagementApi")
)

gaCollection <- setRefClass(
  "gaCollection",
  contains = c(".googleApiCollection", ".gaManagementApi")
)

gaUserSegment <- setRefClass(
  "gaUserSegment",
  contains = "gaResource",
  fields = list(
    type = "character",
    segmentId = "character",
    definition = "character"
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
      type <<- character(0)
      segmentId <<- character(0)
      definition <<- character(0)
      parent_class_name <<- "NULL"
      request <<- "segments"
    },
    get = function() {
      if (!is.null(req_path)) {
        user_segment_fields <- subset(
          gaUserSegments$new(creds = creds)$summary,
          subset = id == id
        )
        .self$modify(user_segment_fields)
      }
      .self
    },
    UPDATE = function(){},
    api_list = function(){
      c(callSuper(), list(
        type = type,
        segmentId = segmentId,
        definition = definition
      ))
    },
    field_corrections = function(field_list) {
      mutate(
        callSuper(field_list),
        type = factor(type, levels = user_segment_type_levels)
      )
    }
  )
)

gaUserSegments <- setRefClass(
  "gaUserSegments",
  contains = ".gaCollection",
  methods = list(
    INSERT = function(){},
    DELETE = function(){},
    initialize = function(...){
      callSuper(...)
      entity_class_generator <<- gaUserSegment
      request <<- "segments"
    },
    field_corrections = function(field_list) {
      mutate(
        callSuper(field_list),
        type = factor(type, levels = user_segment_type_levels)
      )
    }
  )
)
