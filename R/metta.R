meta_api <- "https://www.googleapis.com/analytics/v3/metadata"
report_type <- "ga/columns"
query <- NULL

meta_data <- GaApiRequest(baseURL = meta_api, request = report_type, query = query)

vars <- meta_data$items

df <- ldply(vars, function(var) {data.frame(var$attributes[c('type', 'uiName')])})

