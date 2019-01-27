#' ga_view_selector
#'
#' GaView A menu user-interface for selecting a Google Analytics view.
#'
#' @param creds Optional. An OAuth2.0 credentials object to use for the request.
#' @param with_gui Optional. Boolean value indicating whether to use a GUI for
#'   the menu. Default is FALSE.
#'
#' @return A gaView object.
#'
#' @export
#' @rdname GaView
ga_view_selector <- function(creds = GoogleApiCreds(), with_gui = FALSE) {
  assert_that(
    length(with_gui) == 1L,
    with_gui %in% c(TRUE, FALSE)
  )
  ga_accounts <- GaAccounts(creds = creds)
  ga_accounts_df <- ga_accounts$summary[c('id', 'name', 'starred')]
  account_choice_labels <- paste0(
    ga_accounts_df$id, ": ",
    ga_accounts_df$name,
    ifelse(ga_accounts_df$starred %in% "TRUE", " *", "")
  )
  ga_account_index <- menu(
    choices = account_choice_labels,
    title = "Please choose a GA account",
    graphics = with_gui
  )
  stopifnot(ga_account_index > 0)

  ga_properties <- ga_accounts[[ga_account_index]]$properties
  ga_properties_df <- ga_properties$summary[c('id', 'name', 'websiteUrl')]
  property_choice_labels <- paste0(
    ga_properties_df$id, ": ",
    ga_properties_df$name, " ",
    "(", ga_properties_df$websiteUrl, ")"
  )
  ga_property_index <- menu(
    choices = property_choice_labels,
    title = "Please choose a GA property",
    graphics = with_gui
  )
  stopifnot(ga_property_index > 0)

  ga_views <- ga_properties[[ga_property_index]]$views
  ga_views_df <- ga_views$summary[c('id', 'name', 'type', 'starred')]
  view_choice_labels <- paste0(
    ga_views_df$id, ": ",
    ga_views_df$name, " ",
    "(", ga_views_df$type, ")",
    ifelse(ga_views_df$starred %in% "TRUE", " *", "")
  )
  ga_view_index <- menu(
    choices = view_choice_labels,
    title = "Please choose a GA view",
    graphics = with_gui
  )
  stopifnot(ga_view_index > 0)

  ga_views[[ga_view_index]]
}

#' GaView
#'
#' Get the viewId of the query
#'
#' @param object An object to coerce to a gaView class object or to get the
#'   gaView of, such as a query, default view of a web property, or the default
#'   view of the first web property in a Google Analytics account.
#' @param value The optional replacement view if the object supplied is a query,
#'   in which case GaView will return the modified query.
#'
#' @export
#' @rdname GaView
setGeneric(
  "GaView",
  function(object, value) {standardGeneric("GaView")},
  valueClass = c("viewId", "gaView", ".query")
)

#' GaView<-
#'
#' Set the viewId for the query.
#'
#' @export
#' @rdname GaView
setGeneric(
  "GaView<-",
  function(object, value) {
    object <- standardGeneric("GaView<-")
    validObject(object)
    object
  }
)
