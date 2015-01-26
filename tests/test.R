# Using default credientials supplied via environment variables
# or using a app creds file if located in current directory
test_accounts <- GaAccounts()
test_account <- test_accounts$entities[[1]]
test_properties <- test_account$properties
test_property <- test_properties$entities[[1]]

# Passing a property object to GaQuery will select the default view of that property
test_query <- GaQuery(test_property)
test_get_report <- GetGaData(test_query)

test_expr <- GaExpr("medium", "=", "organic")
GaFilter(test_query) <- test_expr
test_get_report <- GetGaData(test_query)
