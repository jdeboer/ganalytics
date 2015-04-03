# Using default credientials supplied via environment variables
# or using a app creds file if located in current directory

library(ganalytics)

creds <- GoogleApiCreds(appCreds = ".app_oauth_creds.json")

test_query <- GaQuery(creds = creds)
myQuery <- test_query

period <- c("2015-03-02", "2015-03-09")
GaDateRange(myQuery) <- period

GaMetrics(myQuery) <- c("ga:impressions","ga:adClicks","ga:adCost","ga:transactions")
GaDimensions(myQuery) <- c("ga:campaign")
segment <- c("gaid::-4")
GaSegment(myQuery) <- segment



filtro_vuelos <- GaExpr("ga:campaign", "=@", "VUE")
filtro_no_rmkt <- GaExpr("ga:campaign", "!@", "RMKT")
filtro_no_marca <- GaExpr("ga:campaign", "!@", "MARCA")

my_filter <- GaAnd(filtro_vuelos, filtro_no_rmkt)

GaFilter(myQuery) <- my_filter

GaMaxResults(myQuery) <- 15000

gadata <- GetGaData(myQuery)
summary(gadata)






test_get_report <- GetGaData(test_query)

test_expr <- GaExpr("medium", "=", "organic")
GaFilter(test_query) <- test_expr
test_get_report <- GetGaData(test_query)

GaFilter(test_query) <- NULL

my_segments <- GaUserSegments(creds = creds)
GaSegment(test_query) <- my_segments$entities[[1]]

creds <- GoogleApiCreds("analytics@lovesdata.net")

library(plyr)

my_gtm_accounts <- GtmAccounts(creds)


df <- ldply(my_gtm_accounts$entities, function(account) {
  ldply(account$containers$entities, function(container) {
    usage_context <- unlist(container$usageContext)
    if(is.null(usage_context)) usage_context <- NA
    data.frame(
      account_name = account$name,
      container_name = container$name,
      usage_context = usage_context
    )
  })
})

account_list <- my_gtm_accounts$entities
gtm_users <- ldply(seq_along(account_list), function(account_i) {
  cat(paste(account_i, "of", length(account_list), "\n"))
  users <- account_list[[account_i]]$permissions
  if (is(users, "gtmPermissions")) {
    users_df <- data.frame(
      permissionId = users$summary$permissionId,
      emailAddress = users$summary$emailAddress,
      accountAccess = unlist(users$summary$accountAccess$permission),
      stringsAsFactors = FALSE
    )
    user_container_access <- users$summary$containerAccess
  } else {
    print(users)
    users_df <- data.frame(
      permissionId = NA,
      emailAddress = NA,
      accountAccess = NA
    )
    user_container_access <- list(data.frame(
      containerId = NA,
      permission = NA
    ))
  }
  user_container_access <- llply(seq_along(user_container_access), function(user_i) {
    cbind(user_container_access[[user_i]], users_df[user_i, ])
  })
  containers <- account_list[[account_i]]$containers
  if (is(containers, "gtmContainers")) {
    container_users <- alply(users_df, 1, function(user_row) {
      data.frame(
        user_row,
        containerId = containers$summary$containerId,
        containerPublicId = containers$summary$publicId,
        containerName = containers$summary$name,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })
  } else {
    print(containers)
    container_users <- list(data.frame(
      users_df,
      containerId = NA,
      containerPublicId = NA,
      containerName = NA
    ))
  }
  permission_levels <- ganalytics:::gtm_container_permission_levels
  names(permission_levels) <- permission_levels
  df <- ldply(seq_along(user_container_access), function(i) {
    permissions <- user_container_access[[i]]$permission
    user_container_access[[i]]$permission <- NULL
    split_permissions <- llply(permission_levels, function(level) {
      laply(permissions, function(permission_set) {
        level %in% permission_set
      })
    })
    split_permissions <- as.data.frame(split_permissions)
    user_container_access[[i]] <- cbind(user_container_access[[i]], split_permissions)
    ret <- merge(container_users[[i]], user_container_access[[i]], by = c("containerId", "permissionId", "emailAddress", "accountAccess"), all = TRUE)
    ret[!is.na(ret$containerId), ]
  })
  unique(cbind(
    accountId = account_list[[account_i]]$id,
    accountName = account_list[[account_i]]$name,
    df
  ))
})

write.csv(gtm_users, file = "~/analytics_gtm_users.csv", row.names = FALSE)

library(stringr)

my_ga_accounts <- GaAccounts(creds)

account_list <- my_ga_accounts$entities#[10]
view_users <- ldply(seq_along(account_list), function(account_i) {
  property_list <- account_list[[account_i]]$properties$entities
  property_list <- property_list#[11]
  ldply(seq_along(property_list), function(property_i) {
    view_list <- property_list[[property_i]]$views$entities
    ldply(seq_along(view_list), function(view_i) {
      cat(paste(account_i, "of", length(account_list), ", "))
      cat(paste(property_i, "of", length(property_list), ", "))
      cat(paste(view_i, "of", length(view_list), "\n"))
      users <- view_list[[view_i]]$users
      if(identical(users, "client error: (403) Forbidden")) {
        df <- data.frame(
          email = NA,
          created = NA,
          updated = NA
        )
        permissions <- NA
      } else if(is(users, "gaViewUserLinks")) {
        df <- data.frame(
          email = users$summary$userRef$email,
          created = users$summary$created,
          updated = users$summary$updated,
          stringsAsFactors = FALSE
        )
        permissions <- users$summary$permissions$effective
      } else {
        print(users)
        browser()
      }
      permission_levels <- ganalytics:::user_permission_levels
      names(permission_levels) <- permission_levels
      split_permissions <- llply(permission_levels, function(level) {
        laply(permissions, function(permission_set) {
          level %in% permission_set
        })
      })
      split_permissions <- as.data.frame(split_permissions)
      df <- cbind(df, split_permissions)
      df <- mutate(
        df,
        accountId = account_list[[account_i]]$id,
        accountName = account_list[[account_i]]$name,
        propertyId = property_list[[property_i]]$id,
        propertyName = property_list[[property_i]]$name,
        viewId = view_list[[view_i]]$id,
        viewName = view_list[[view_i]]$name
      )
      df
    })
  })
})

view_users <- mutate(
  view_users,
  accountId = factor(accountId),
  propertyId = factor(propertyId),
  viewId = factor(viewId),
  viewName = factor(viewName),
  email = factor(email),
  domain = factor(str_split_fixed(as.character(email), "@", 2)[, 2]),
  READ_AND_ANALYZE = factor(READ_AND_ANALYZE),
  COLLABORATE = factor(COLLABORATE),
  EDIT = factor(EDIT),
  MANAGE_USERS = factor(MANAGE_USERS),
  accountName = factor(accountName),
  propertyName = factor(propertyName)
)

view_users <- view_users[c(
  "accountId", "accountName",
  "propertyId", "propertyName",
  "viewId", "viewName",
  "email", "domain", "created", "updated",
  "READ_AND_ANALYZE", "COLLABORATE", "EDIT", "MANAGE_USERS"
)]

saveRDS(my_accounts, "~/my_accounts.RDS")

write.csv(view_users, file = "~/analytics_users.csv", row.names = FALSE)

###

# Testing methods that add, delete or modify Management API data

accounts_summary <- GaAccountSummaries(creds)$summary
views_summary <- adply(accounts_summary, 1, function(account) {
  if (length(account$webProperties[[1]]) >= 1) {
    df <- adply(account$webProperties[[1]], 1, function(property) {
      if (length(property$profiles[[1]]) >= 1) {
        df <- adply(property$profiles[[1]], 1, function(view) {
          df <- summarise(
            view,
            account_name = account$name,
            property_name = property$name,
            tracker_id = property$id,
            level = property$level,
            view_name = name,
            view_id = id,
            type = type
          )
        }, .expand = FALSE)
      }
    }, .expand = FALSE)
  }
}, .expand = FALSE)
views_summary <- summarise(views_summary,
                           account_name = factor(account_name),
                           property_name = factor(property_name),
                           tracker_id = factor(tracker_id),
                           level = factor(level),
                           view_name = view_name,
                           view_id = view_id,
                           type = factor(type))

app_views <- subset(views_summary, subset = type == "APP")

creds <- GoogleApiCreds("demo@lovesdata.net")
my_accounts <- GaAccounts(creds)
my_account <- my_accounts$get_entity(id = 32554188) # 33109290
my_account
