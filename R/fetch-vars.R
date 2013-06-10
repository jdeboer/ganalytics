library(httr)
library(XML)
library(plyr)
library(stringr)

# At present the Dims and Mets reference guide does not mention
# the isTablet dimension.

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
    cat = xmlValue(node),
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
  cat = factor(cat)
)

# Get the descriptions for each dimension and metric along
# with indicator of whether a metric is calculated.
entities <- ddply(entities, "page", function(page_entities){
  url <- paste(base_url, unique(page_entities$page), sep = "/")
  res <- GET(url)
  pars <- htmlTreeParse(res, useInternalNodes = TRUE)
  
  # Get the main content of the category page
  cat_xpath <- "//div[@itemprop = \"articleBody\"]"
  cat_node <- getNodeSet(pars, cat_xpath)[[1]]
  
  # Get the names of dim/met names for each description given
  desc_name_xpath <- "//h3[starts-with(@id, \"ga:\")]/text()"
  desc_name_nodes <- getNodeSet(cat_node, desc_name_xpath)
  desc_names <- xmlSApply(desc_name_nodes, xmlValue)
  
  # Get the description that follows each dim/met name, removing
  # new lines, leading and consecutive spaces.
  desc_xpath <- "//h3[starts-with(@id, \"ga:\")]/following-sibling::div"
  desc_nodes <- getNodeSet(cat_node, desc_xpath)
  descriptions <- str_trim(xmlSApply(desc_nodes, xmlValue))
  descriptions <- str_replace_all(descriptions, "\\n", " ")
  descriptions <- str_replace_all(descriptions, "[ ]{2,}", " ")
  
  # Apply the names to each of the descriptions and add the matching
  # the descriptions to the data.frame.
  names(descriptions) <- desc_names
  page_entities$description <- descriptions[page_entities$var]
  
  # Identify calculated metrics
  calc_xpath <- "//*[@href = \"#calc\"]/following-sibling::*//a/text()"
  calc_metrics <- xpathSApply(cat_node, calc_xpath, xmlValue)
  
  # If any calculated metrics
  if(length(calc_metrics) > 0) {
    page_entities$type <- as.character(page_entities$type)
    page_entities[page_entities$var %in% calc_metrics, 'type'] <- "cal"
    page_entities$type <- factor(page_entities$type)
  }
  
  # Return the resulting data.frame for that particular category page
  page_entities
}, .progress = "time")

write.csv(entities, file = "~/gavars.csv")
