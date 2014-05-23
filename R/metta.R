meta_api <- "https://www.googleapis.com/analytics/v3/metadata"
report_type <- "ga/columns"
query <- NULL

oauth <- GaAuth()

meta_data <- ganalytics:::GaApiRequest(baseURL = meta_api, request = report_type, query = query, oauth = oauth)

vars <- meta_data$items

df <- ldply(vars, function(var) {data.frame(var$attributes[c('type', 'uiName')])})

