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

# Extract a list of dimensions and metrics separately for each category.
dims_xpath <- "./*[@class=\"dim\"]/*/a"
mets_xpath <- "./*[@class=\"met\"]/*/a"
entities <- ldply(entity_table_nodeSet, function(node){
  dims_nodeSet <- getNodeSet(node, dims_xpath)
  dims <- ldply(dims_nodeSet, function(node){
    df <- data.frame(
      var = xmlValue(node),
      type = "dim",
      href = str_replace(xmlGetAttr(node, "href"), ".*/", ""),
      stringsAsFactors = FALSE
    )
    href <- str_split_fixed(df$href, "#", 2)
    df$href <- NULL
    df$page <- href[,1]
    df
  })
  mets_nodeSet <- getNodeSet(node, mets_xpath)
  mets <- ldply(mets_nodeSet, function(node){
    df <- data.frame(
      var = xmlValue(node),
      type = "met",
      href = str_replace(xmlGetAttr(node, "href"), ".*/", ""),
      stringsAsFactors = FALSE
    )
    href <- str_split_fixed(df$href, "#", 2)
    df$href <- NULL
    df$page <- href[,1]
    df
  })
  entity_table <- rbind(dims, mets)
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
  cat_xpath <- "//div[@itemprop = \"articleBody\"]"
  cat_node <- getNodeSet(pars, cat_xpath)[[1]]
  desc_name_xpath <- "//h3[starts-with(@id, \"ga:\")]/text()"
  desc_name_nodes <- getNodeSet(cat_node, desc_name_xpath)
  desc_names <- xmlSApply(desc_name_nodes, xmlValue)
  desc_xpath <- "//h3[starts-with(@id, \"ga:\")]/following-sibling::div"
  desc_nodes <- getNodeSet(cat_node, desc_xpath)
  descriptions <- str_trim(xmlSApply(desc_nodes, xmlValue))
  descriptions <- str_replace_all(descriptions, "\\n", " ")
  descriptions <- str_replace_all(descriptions, "[ ]{2,}", " ")
  names(descriptions) <- desc_names
  calc_xpath <- "//*[@href = \"#calc\"]/following-sibling::*//a/text()"
  calc_metrics <- xpathSApply(cat_node, calc_xpath, xmlValue)
  page_entities$type <- as.character(page_entities$type)
  if(length(calc_metrics) > 0) {
    page_entities[page_entities$var %in% calc_metrics, 'type'] <- "cal"
  }
  page_entities$description <- descriptions[page_entities$var]
  page_entities
}, .progress = "time")

write.csv(entities, file = "~/gavars.csv")
