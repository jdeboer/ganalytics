#' @importFrom httr oauth_endpoints oauth1.0_token oauth2.0_token config 
#'   stop_for_status content oauth_app modify_url add_headers GET POST PUT
#'   DELETE
#' @importFrom stringr str_c str_detect str_replace_all
#' @importFrom plyr aaply
#' @importFrom XML xmlParse xmlToList xmlApply
#' @importFrom selectr querySelectorAll
#' @importFrom jsonlite toJSON fromJSON validate
NULL

#' @export
GoogleApiCreds <- function(
  userName = character(0),
  appCreds = NULL,
  cache = character(0),
  use_oob = FALSE,
  appname = "GOOGLE_APIS"
){
  cache_generic_file_name <- paste0(tolower(appname), "_auth.RDS")
  cache_file_prefix <- "."
  cache_default_dir <- "~"
  if (length(cache) == 0) {
    if (length(userName) == 0) {
      cache <- cache_generic_file_name
    } else {
      cache <- paste0(userName, "_", cache_generic_file_name)
    }
    cache <- paste0(cache_default_dir, "/", cache_file_prefix, cache)
  }
  list(
    app = app_oauth_creds(
      appname = appname,
      creds = appCreds
    ),
    user = list(
      login = userName,
      cache = cache
    ),
    use_oob = use_oob
  )
}

google_api_request <- function(creds, scope,
                               request, base_url,
                               queries = NULL, req_type = "GET",
                               body_list = NULL, fields = NULL,
                               max_results = NULL) {
  api_name <- "google"
  api_request(
    api_name = api_name,
    app = creds$app,
    base_url = paste(c(base_url, request), collapse = "/"),
    scope = scope,
    req_type = req_type,
    queries = c(
      queries,
      fields = parse_field_list(fields)
    ),
    body_list = body_list,
    user = creds$user,
    use_oob = creds$use_oob
  )
}

api_request <- function(api_name, app, base_url,
                        scope = NULL, req_type = "GET",
                        queries = NULL, body_list = NULL,
                        user = list(login = NA, cache = NA),
                        oauth_in_header = TRUE,
                        use_oob = FALSE,
                        oauth_version = "2.0") {
  req_type <- toupper(req_type)
  api_name <- tolower(api_name)
  stopifnot(
    req_type %in% c("GET", "POST", "PUT", "DELETE")
  )
  url <- form_url(base_url, queries)
  endpoint <- oauth_endpoints(name = api_name)
  if(length(user$login) != 1) {user$login <- NA}
  if(!is.na(user$login)) {
    endpoint$authorize <- modify_url(
      endpoint$authorize,
      query = list(login_hint = user$login)
    )
  }
  scope <- if(!is.null(scope)) {
    paste(scope, collapse = " ")
  }
  switch(
    oauth_version,
    `1.0` = {
      token <- oauth1.0_token(
        endpoint = endpoint,
        app = app,
        permission = scope,
        cache = user$cache
      )
    },
    `2.0` = {
      token <- oauth2.0_token(
        endpoint = endpoint,
        app = app,
        scope = scope,
        use_oob = use_oob,
        as_header = oauth_in_header,
        cache = user$cache
      )
    }
  )
  args <- list(
    url = url,
    config = config(token = token)
  )
  if(!is.null(body_list)) {
    body <- toJSON(
      body_list,
      pretty = TRUE#, asIs = TRUE #, auto_unbox = TRUE
    )
    args$config <- c(
      args$config,
      add_headers(
        `Content-Type` = "application/json"
      )
    )
    args <- c(args, list(body = body))
  }
  
  attempts <- 0
  succeeded <- FALSE
  while (attempts <= 5 & !succeeded) {
    attempts <- attempts + 1
    response <- do.call(req_type, args)
    json_content <- response_to_list(response)
    if (any(json_content$error$errors$reason %in% c('userRateLimitExceeded', 'quotaExceeded'))) {
      Sys.sleep((2 ^ attempts) + runif(n = 1, min = 0, max = 1))
    } else {
      message(json_content$error$message)
      stop_for_status(response)
      succeeded <- TRUE
    }
  }
  json_content
}

form_url <- function(base_url, queries = NULL) {
  paste(
    c(
      base_url,
      if(length(queries) >= 1) {
        paste(
          aaply(seq_along(queries), 1, function(query_index){
            query <- queries[query_index]
            paste(names(queries)[query_index], URLencode(as.character(query), reserved = TRUE), sep = "=")
          }),
          collapse = "&"
        )
      }
    ),
    collapse = "?"
  )
}

response_to_list <- function(response) {
  content_type <- response$headers$`content-type`
  if (length(content(response)) >= 1) {
    response_text <- content(x = response, as = "text")
    if (str_detect(content_type, "^application/json;")) {
      return(fromJSON(response_text))
    } else if (str_detect(content_type, "^application/atom\\+xml;")) {
      xml_doc <- xmlParse(response_text, asText = TRUE)
      feed <- querySelectorAll(doc = xml_doc, selector = "x|feed", ns = "x")
      ret_list <- lapply(feed, function(feed_node){
        xmlApply(feed_node, function(node){
          xmlToList(node)
        })
      })
      ret_list <- ret_list[[1]]
      ret_list <- c(
        ret_list[names(ret_list)!="entry"],
        entries = list(
          ret_list[names(ret_list)=="entry"]
        )
      )
      return(ret_list)
    }
  }
  return(response)
}

app_oauth_creds <- function(appname, creds = NULL) {
  if (typeof(creds) == "character" & length(creds) == 1) {
    if(validate(creds)) {
      creds <- fromJSON(creds)
    } else if (file.exists(creds)) {
      creds <- fromJSON(creds)
    } else {
      creds <- NULL
    }
    if("installed" %in% names(creds)) {
      creds <- creds$installed
    }
  }
  if (typeof(creds) != "list") {
    if (length(creds) == 2 &
          identical(all(names(creds) %in% c("client_id", "client_secret")), TRUE)) {
      creds <- as.list(creds)
    } else {
      creds <- list(client_id = NULL, client_secret = NULL)
    }
  }
  if (is.null(creds$client_id)) {
    creds$client_id <- Sys.getenv(str_c(toupper(appname), "_CONSUMER_ID"))
  }
  if (is.null(creds$client_secret)) {
    creds$client_secret <- Sys.getenv(str_c(toupper(appname), "_CONSUMER_SECRET"))
  }
  oauth_app(
    appname = appname,
    key = creds$client_id,
    secret = creds$client_secret
  )
}

# This function accepts a recursive list of list elements, each named to represent a field,
# and converts this into a field
# e.g. 
# field_list <- list(
#   "a" = list(),
#   "b" = list(
#     "*" = list(
#       "f" = list(),
#       "z" = list()
#     ),
#     "c" = list(),
#     "d" = list()
#   ),
#   "e" = list()
# )
parse_field_list <- function(field_list) {
  # A list must be provided, and as this function is recursive,
  # all elements within that list must also be lists, and so on.
  if(length(field_list) == 0) {
    return(NULL)
  }
  stopifnot(
    is.list(field_list)
  )
  # for each element within the list...
  fields <- laply(seq_along(field_list), function(field_index) {
    # Get the name and content of the field element
    field_name <- names(field_list)[field_index]
    field_content <- field_list[[field_index]]
    # Get the legnth of the field's content
    field_length <- length(field_content)
    # if that element is a end node then
    if (field_length == 0) {
      # return the name of that node
      return(field_name)
    } else {
      # otherwise recursively parse each of its sub elements
      sub_fields <- parse_field_list(field_content)
      # if there was more than one sub element, then group those sub elements
      if(field_length > 1) {
        sub_fields <- paste0("(", sub_fields, ")")
      }
      # the sub fields are returned with a preceeding "/"
      return(paste0(
        field_name, "/", sub_fields
      ))
    }
  })
  # each of the fields must be separated by ","
  parsed_fields <- paste0(fields, collapse = ",")
  return(parsed_fields)
}

get_privates <- function(class_gen){
  with(class_gen, c(private_fields, private_methods))
}

.googleApi <- R6Class(
  ".googleApi",
  public = list(
    creds = GoogleApiCreds(),
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
    initialize = function(creds = GoogleApiCreds()) {
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
      if(is.data.frame(field_list)) {
        if(exists("created", field_list)) {
          field_list$created <- ymd_hms(field_list$created)
        }
        if(exists("updated", field_list)) {
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
    initialize = function(creds = GoogleApiCreds(), parent = NULL, id = NA) {
      super$initialize(creds = creds)
      stopifnot(is(parent, private$parent_class_name) | is(parent, "NULL"))
      self$parent <- parent
      self$id <- id
      if(!is.na(id)) {
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
        req_type = "DELETE",
      )
      self$get()
    },
    initialize = function(creds = GoogleApiCreds(), parent = NULL) {
      super$initialize(creds = creds)
      entity_class_private <- get_privates(private$entity_class)
      private$request <- entity_class_private$request
      private$parent_class_name <- entity_class_private$parent_class_name
      stopifnot(is(parent, private$parent_class_name) | is.null(parent))
      if(is.null(private$collection_name)) {
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
