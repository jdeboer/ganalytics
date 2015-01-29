#'@include ganalytics-package.R
NULL

#Make a Goolge API request
ga_api_request <- function(
  creds,
  request,
  scope = ga_scopes["read_only"],
  req_type = "GET",
  body_list = NULL,
  fields = NULL,
  queries = NULL
) {
  stopifnot(scope %in% ga_scopes)
  base_url <- "https://www.googleapis.com/analytics/v3"
  request <- c(
    "upload"[attr(request, "upload")],
    request
  )
  google_api_request(
    creds = creds,
    scope = scope,
    request = request,
    base_url = base_url,
    queries = queries,
    req_type = req_type,
    body_list = body_list,
    fields = fields
  )
}

gtm_api_request <- function(
  creds,
  request,
  scope = gtm_scopes["read_only"],
  req_type = "GET",
  body_list = NULL,
  fields = NULL
) {
  stopifnot(scope %in% gtm_scopes)
  base_url <- "https://www.googleapis.com/tagmanager/v1"
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

google_api_request <- function(creds, scope,
                               request, base_url,
                               queries = NULL, req_type = "GET",
                               body_list = NULL, fields = NULL) {
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
            query <- str_replace_all(query, "\\+", "%2B")
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
  }
  if (typeof(creds) != "list") {
    if (length(creds) == 2) {
      if (names(creds) == c("client_id", "client_secret")) {
        creds <- as.list(creds)
      } else {
        creds <- list(client_id = NULL, client_secret = NULL)
      }
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
