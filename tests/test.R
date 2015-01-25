# Using default credientials supplied via environment variables
# or using a app creds file if located in current directory
test_accounts <- gaAccounts$new()
test_account <- test_accounts$entities[[1]]
test_properties <- test_account$properties
test_property <- test_properties$entities[[1]]
test_view <- test_property$defaultView
test_view_id <- test_view$id
test_query <- GaQuery(test_view_id)
test_get_report <- GetGaData(test_query)
GaExpr("medium", "=", "organic")
GaExpr("hostname", "~", "")
GaFilter(test_query) <- test_expr
test_get_report <- GetGaData(test_query)
