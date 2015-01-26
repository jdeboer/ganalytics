# Using default credientials supplied via environment variables
# or using a app creds file if located in current directory

test_query <- GaQuery()
test_get_report <- GetGaData(test_query)

test_expr <- GaExpr("medium", "=", "organic")
GaFilter(test_query) <- test_expr
test_get_report <- GetGaData(test_query)
