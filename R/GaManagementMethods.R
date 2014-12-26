#'@include ga-management-classes.R

GaAccounts <- function(ids = NULL) {
  accounts <- llply(ids, function(id) {
    new("Account", id = as.character(id), name = "Unknown")
  })
  new("Accounts", accounts)
}

GaProperties <- function(account, ids = NULL) {
  properties <- llply(ids, function(id) {
    new("Property", id = as.character(id), name = "Unknown", belongs_to = account)
  })
  new("Properties", properties, belongs_to = account)
}

GaViews <- function(property, ids = NULL) {
  views <- llply(ids, function(id) {
    new("View", id = as.character(id), name = "Unknown", belongs_to = property)
  })
  new("Views", views, belongs_to = property)
}

#'@export
GetGaAccounts <- function(creds) {
  items <- GetGaData(GaAccounts(), creds)$items
  accounts <- alply(items, 1, function(account) {    
    new("Account",
      id = account$id,
      name = account$name,
      created = ymd_hms(account$created),
      updated = ymd_hms(account$updated)
    )
  })
  new("Accounts", accounts)
}

#'@export
GetGaProperties <- function(account, creds) {
  items <- GetGaData(GaProperties(account), creds)$items
  properties <- alply(items, 1, function(property) {    
    new("Property",
      id = property$id,
      name = property$name,
      created = ymd_hms(property$created),
      updated = ymd_hms(property$updated),
      belongs_to = account,
      websiteUrl = property$websiteUrl,
      industryVertical = property$industryVertical,
      defaultView = property$defaultProfileId
    )
  })
  new("Properties", properties)
}

#'@export
GetGaViews <- function(property, creds) {
  items <- GetGaData(GaViews(property), creds)$items
  views <- alply(items, 1, function(view) {    
    new("View",
      id = view$id,
      name = view$name,
      created = ymd_hms(view$created),
      updated = ymd_hms(view$updated),
      belongs_to = property,
      type = factor(view$type, levels = view_type_levels),
      currency = factor(view$currency, levels = currency_levels),
      timezone = as.character(view$timezone),
      websiteUrl = as.character(view$websiteUrl),
      defaultPage = as.character(view$defaultPage),
      excludeQueryParameters = as.character(view$excludeQueryParameters),
      siteSearchQueryParameters = as.character(view$siteSearchQueryParameters),
      stripSiteSearchQueryParameters = identical(view$stripSiteSearchQueryParameters, TRUE),
      siteSearchCategoryParameters = as.character(view$siteSearchCategoryParameters),
      stripSiteSearchCategoryParameters = identical(view$stripSiteSearchCategoryParameters, TRUE),
      eCommerceTracking = view$eCommerceTracking#,
      #enhancedECommerceTracking
    )
  })
  new("Views", views)
}

