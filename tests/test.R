# Using default credientials supplied via environment variables
# or using a app creds file if located in current directory

test_query <- GaQuery()
test_get_report <- GetGaData(test_query)

test_expr <- GaExpr("medium", "=", "organic")
GaFilter(test_query) <- test_expr
test_get_report <- GetGaData(test_query)

GaFilter(test_query) <- NULL

my_segments <- GaUserSegments()
GaSegment(test_query) <- my_segments$entities[[1]]

creds <- GaCreds("analytics@lovesdata.net")
my_account_summaries <- GaAccountSummaries(creds)

nrow(my_account_summaries$summary)

names(my_account_summaries$summary$webProperties[[1]]$profiles[[1]])
