library(lubridate)
library(plyr)
library(assertthat)
library(jsonlite)
library(httr)
library(stringr)
library(R6)
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

gaManagementApi <- R6Class(
  "gaManagementApi",
  public = list(
    get = function(max_results = NULL) {
      req_type <- "GET"
      private$api_req_func(
        creds = self$creds,
        request = self$req_path,
        scope = private$scopes$read,
        base_url = private$base_url,
        req_type = req_type,
        max_results = max_results
      )
    }
  ),
  active = list(
    req_path = function() {
      NULL
    },
    creds = function(value) {
      if (missing(value)) {
        private$creds
      } else {
        private$creds <- value
      }
    }
  ),
  private = list(
    creds = get_creds(),
    scopes = list(
      read = ga_scopes['read_only'],
      write = ga_scopes["edit"]
    ),
    base_url = "https://www.googleapis.com/analytics/v3",
    api_req_func = function(request, ...){
      request <- c(
        "management",
        "upload"[attr(request, "upload")],
        request
      )
      ga_api_request(request = request, ...)
    }
  )
)

setOldClass(c("gaManagementApi", "R6"))

gaResource <- R6Class(
  "gaResource",
  inherit = gaManagementApi,
  public = list(
    get = function(max_results = NULL) {
      if (length(self$req_path) > 0) {
        super$get(max_results = max_results)
      }
      self
    },
    UPDATE = function() {
      self$get()
    }
  ),
  active = list(
    id = function() {
      private$id
    },
    name = function(value) {
      if (missing(value)) {
        private$name
      } else {
        value <- as.character(value)
        assert_that(length(value) == 1)
        private$name <- value
      }
    },
    created = function() {
      private$created
    },
    updated = function() {
      private$updated
    },
    field_summary = function() {
      list(
        id = self$id,
        name = self$name,
        created = self$created,
        updated = self$updated
      )
    },
    parent_entity = function() {
      private$parent_entity
    },
    req_path = function(){
      if (is.na(self$id) | length(self$id) == 0) {
        NULL
      } else {
        c(
          if (is(parent_entity, "gaResource")) {
            self$parent_entity$req_path
          },
          private$collection_name,
          URLencode(self$id, reserved = TRUE)
        )
      }
    }
  ),
  private = list(
    id = as.character(NA),
    name = as.character(NA),
    created = as.POSIXct(NA),
    updated = as.POSIXct(NA),
    parent_entity = NULL,
    resource_name = "itmes",
    collection_name = character(0),
    parent_class_name = "NULL"
  )
)

setOldClass(c("R6ClassGenerator", "R6"))
setOldClass(c("gaManagementApi", "R6"))
setOldClass(c("gaResource", "R6"))

gaResourceOrNULL <- setClassUnion(
  "gaResourceOrNULL",
  c("gaResource", "NULL")
)

gaCollection <- setClass(
  "gaCollection",
  contains = "list",
  slots = list(
    resource_class_generator = "R6ClassGenerator",
    summary = "data.frame"
  ),
  validity = function(object) {
    validate_that(
      all_inherit(object, "gaResource"),
      all_inherit(object, object@resource_class_generator$className),
      nrow(object@summary) == length(object@.Data)
    )
  }
)

setMethod("initialize", "gaCollection", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@summary <- ldply(.Object, function(entity) {
    as.data.frame(entity$entity_field_values)
  })
  .Object
})

setGeneric("req_path", function(object) {
  standardGeneric("req_path")
}, valueClass = "character")

setMethod("req_path", "gaCollection", function(object) {
  c(
    if (is(object@parent_entity, "gaResource")) {
      object@parent_entity$req_path
    },
    object@collection_name
  )
})

gaAccount <- R6Class(
  "gaAccount",
  inherit = "gaResource",
  private = list(
    collection_name = "accounts"
  ),
  active = list(
    properties = function() {

    },
    filters = function() {

    },
    users = function() {

    }
  )
)

setOldClass(c("gaAccount", "R6"))

gaAccounts <- setClass(
  "gaAccounts",
  contains = "gaCollection"
)

setMethod("initialize", "gaAccounts", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@parent_entity <- NULL
  .Object@entity_class_generator <- gaAccount
  .Object@collection_name <- "accounts"
  callNextMethod()
})

setGeneric("GET_LIST", function(collection) {

  ###
  # $kind
  # [1] "analytics#accounts"
  #
  # $username
  # [1] "johann@lovesdata.com"
  #
  # $totalResults
  # [1] 43
  #
  # $startIndex
  # [1] 1
  #
  # $itemsPerPage
  # [1] 1000

  collection@api$get()[[collection@resource_name]]
})

setGeneric("GET_ENTITY", function(collection, id) {

})

setGeneric("INSERT_ENTITY", function(collection, entity) {

})

setGeneric("DELETE_ENTITY", function(collection, id, ask_confirmation = TRUE) {

})

setGeneric("UPDATE_ENTITY", function(collection, id, ask_confirmation = TRUE) {

})


