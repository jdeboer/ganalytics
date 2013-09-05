fetchVars <- function() {
  
  library(httr)
  library(XML)
  library(plyr)
  library(stringr)
  
  # At present the Dims and Mets reference guide does not mention
  # the isTablet or dimension(n) type dimensions, or the metric(n) type metrics.
  # https://developers.google.com/analytics/devguides/reporting/core/dimsmets/visitor
  
  # The home page URL of Dimensions and Metrics reference guide
  base_url <- "https://developers.google.com/analytics/devguides/reporting/core/dimsmets"
  # Get the home page, which contains an index of all dimensions
  # and metrics. Parse as an XML document for scraping.
  res <- GET(base_url)
  pars <- htmlTreeParse(res, useInternalNodes = TRUE)
  
  # XPath for the index table of all dims and mets
  table_xpath <- "//*[@id=\"table\"]"
  # There is only one index table, so grab the first one.
  table_node <- getNodeSet(pars, table_xpath)[[1]]
  
  # The categories and the URL to details about each category.
  cats_xpath <- "./*[@class=\"cats\"]/a"
  cats_nodeSet <- getNodeSet(table_node, cats_xpath)
  # Build a table of category pages and their corresponding URLs
  cats <- ldply(cats_nodeSet, function(node){
    data.frame(
      category = xmlValue(node),
      page = str_replace(xmlGetAttr(node, "href"), ".*/", ""),
      stringsAsFactors = FALSE
    )
  })
  
  # Table of all dimensions and metrics and for each category.
  entity_table_xpath <- "./*[@class=\"entity table\"]"
  entity_table_nodeSet <- getNodeSet(table_node, entity_table_xpath)
  
  GetVars <- function(node, type) {
    xpath <- paste("./*[@class=", type, "]/*/a", sep = "\"")
    nodeSet <- getNodeSet(node, xpath)
    ldply(nodeSet, function(node){
      df <- data.frame(
        var = xmlValue(node),
        type = type,
        page = str_replace(xmlGetAttr(node, "href"), ".*/", ""),
        stringsAsFactors = FALSE
      )
      page <- str_split_fixed(df$page, "#", 2)
      df$page <- page[,1]
      df
    })
  }
  
  # Extract a list of dimensions and metrics separately for each category.
  
  entities <- ldply(entity_table_nodeSet, function(node){
    dims <- GetVars(node, "dim")
    mets <- GetVars(node, "met")
    rbind(dims, mets)
  })
  
  # Join the table of variables with details about each category
  entities <- join(entities, cats, "page")
  entities <- mutate(
    entities,
    type = factor(type),
    page = factor(page),
    category = factor(category)
  )
  
  # Get the descriptions for each dimension and metric along
  # with the indicator of whether a metric is calculated.
  entities <- ddply(entities, "page", function(page_entities){
    url <- paste(base_url, unique(page_entities$page), sep = "/")
    res <- GET(url)
    pars <- htmlTreeParse(res, useInternalNodes = TRUE)
    
    # Get the main content of the category page
    cat_xpath <- "//div[@itemprop = \"articleBody\"]"
    cat_node <- getNodeSet(pars, cat_xpath)[[1]]
    
    # Identify calculated metrics
    calc_xpath <- "//*[@href = \"#calc\"]/following-sibling::*//a/text()"
    calc_metrics <- xpathSApply(cat_node, calc_xpath, xmlValue)
  
    # Get the names of dim/met names for each description given
    desc_name_xpath <- "//h3[starts-with(@id, \"ga:\")]/text()"
    desc_name_nodes <- getNodeSet(cat_node, desc_name_xpath)
    desc_names <- xmlSApply(desc_name_nodes, xmlValue)
    
    # Get the description that follows each dim/met name, removing
    # new lines, leading and consecutive spaces.
    desc_xpath <- "//h3[starts-with(@id, \"ga:\")]/following-sibling::div"
    desc_nodes <- getNodeSet(cat_node, desc_xpath)
    descriptions <- str_trim(xmlSApply(desc_nodes, xmlValue))
    # If any calculated metrics then refactorise the type vector to include
    # "cal" as a level, and set all calcualted entities to that type value.
    formulae <- str_match(descriptions, "(\n *){2}Calculation(\n *){2}(.*$)")[,4]
    descriptions <- str_replace(descriptions, "(\n *){2}Calculation(\n *){2}.*$", "")
    descriptions <- str_replace_all(descriptions, "\\n", " ")
    descriptions <- str_replace_all(descriptions, "[ ]{2,}", " ")
    if(length(calc_metrics) > 0) {
      page_entities$type <- as.character(page_entities$type)
      page_entities[page_entities$var %in% calc_metrics, 'type'] <- "cal"
      page_entities$type <- factor(page_entities$type)
    }
    
    # Apply the names to each of the descriptions and add the matching
    # the descriptions to the data.frame.
    names(descriptions) <- desc_names
    names(formulae) <- desc_names
    page_entities$description <- descriptions[page_entities$var]
    page_entities$formula <- formulae[page_entities$var]
    
    # Return the resulting data.frame for that particular category page
    page_entities
  }, .progress = "time")
  
  entities$url <- paste(base_url, entities$page, sep = "/")
  entities$page <- NULL
  
  # For each category group of vars, make queries in sets up to the maximum number
  # of dimensions and metrics to determine what datatypes are returned from each.
  # Ignore calculated metrics
  
  entities_by_category_type <- llply(
    dlply(entities, "category"),
    function(entities_of_category) {
      dlply(entities_of_category, "type")
    }
  )
  
  SplitVars <- function(vars, len) {
    starts <- seq(1, length(vars), by = len)
    if(length(starts) > 1) {
      ends <- seq(starts[2] - 1, length(vars) - 1, by = len)
    } else {
      ends <- NULL
    }
    ends <- c(ends, length(vars))
    ranges <- data.frame(start = starts, end = ends)
    dlply(ranges, "start", function(range) {
      vars[range$start:range$end]
    })
  }
  
  authFile = "~/ganalytics_token.RDS"
  
  oauth <- new_oauth(
    key = NULL,
    secret = NULL,
    file = authFile,
    scope = "https://www.googleapis.com/auth/analytics.readonly",
    base_url = "https://accounts.google.com/o/oauth2",
    authorize = "auth",
    access = "token",
    appname = "GANALYTICS"
  )
  
  dataTypes <- llply(
    entities_by_category_type,
    function(entities_of_category) {
      mets <- c(entities_of_category$met$var, entities_of_category$cal$var)
      mets <- str_replace(mets, "\\(n\\)", "1")
      if(length(mets) >= 1) {
        mets <- SplitVars(mets, 10)
        request <- ""
        gaData <- llply(mets, function(metSet) {
          queryUrl <- paste(
            paste("ids", "ga:52825603", sep = "="),
            paste("start-date", as.character(Sys.Date() - 1), sep = "="),
            paste("end-date", as.character(Sys.Date() - 1), sep = "="),
            paste("metrics", URLencode(paste(metSet, collapse = ","), reserved = TRUE), sep = "="),
            sep = "&"
          )
          data.ga <- GaApiRequest(reporting.api, request, queryUrl, oauth)
          dataTypes <- laply(data.ga$columnHeaders, function(header) header$dataType)
          names(dataTypes) <- laply(data.ga$columnHeaders, function(header) header$name)
          return(dataTypes)
        })
        names(gaData) <- NULL
        gaData <- unlist(gaData)
      }
    },
    .progress = "time"
  )
  
  names(dataTypes) <- NULL
  dataTypes <- unlist(dataTypes)
  names(dataTypes) <- str_replace(names(dataTypes), "1", "(n)")
  
  entities$dataType <- "STRING"
  rownames(entities) <- entities$var
  entities[names(dataTypes), 'dataType'] <- dataTypes
  
  write.csv(entities, file = "~/gavars.csv", row.names = FALSE)
}