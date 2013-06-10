library(httr)
library(XML)
library(plyr)
library(stringr)

base_url <- "https://developers.google.com/analytics/devguides/reporting/core/dimsmets"
res <- GET(base_url)
pars <- htmlTreeParse(res, useInternalNodes = TRUE)

table_xpath <- "//*[@id=\"table\"]"
table_node <- getNodeSet(pars, table_xpath)[[1]]

cats_xpath <- "./*[@class=\"cats\"]/a"
cats_nodeSet <- getNodeSet(table_node, cats_xpath)
cats <- ldply(cats_nodeSet, function(node){
  data.frame(
    cat = xmlValue(node),
    page = str_replace(xmlGetAttr(node, "href"), ".*/", ""),
    stringsAsFactors = FALSE
  )
})

entity_table_xpath <- "./*[@class=\"entity table\"]"
entity_table_nodeSet <- getNodeSet(table_node, entity_table_xpath)

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
    #df$anchor <- href[,2]
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
    #df$anchor <- href[,2]
    df
  })
  entity_table <- rbind(dims, mets)
})

entities <- join(entities, cats, "page")
entities <- mutate(
  entities,
  type = factor(type),
  page = factor(page),
  cat = factor(cat)
)

entities2 <- ddply(entities, "page", function(page_entities){
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
}
, .progress = "time"
)

write.csv(entities2, file = "~/gavars.csv")
