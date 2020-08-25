library(tidyr)
library(dplyr)
library(jsonlite)

# should we get this from config? https://master.bioconductor.org/config.yaml
# versions <- c("3.11", "3.12")
#release_version / devel_version in config.yaml  and/or versions
# or use devel/release instead and get bioc from report


# originally looped over version but for now concentrate on one
#for(ver in versions){

tbl <- read.table(paste0("https://master.bioconductor.org/checkResults/devel/bioc-LATEST/STATUS_DB.txt"), comment.char="")
names(tbl) = c("Builder", "Status")
res <- tbl %>% separate(Builder, c("Package", "Builder", "Stage"), "#")

# git problems as defined by skipped, ERROR, TIMEOUT
idx <- which(tolower(res[,"Status"]) %in% tolower(c("skipped","ERROR",
                                                    "TIMEOUT")))


# helpful tips
#https://www.freecodecamp.org/news/an-introduction-to-web-scraping-using-r-40284110c848/
library(rvest)
library(stringr)
html = read_html("https://bioconductor.org/checkResults/devel/bioc-LATEST/")
nodeSpec <- (html %>% html_nodes('.node_specs') %>% html_table())[[1]]
timeStamp <- (html %>% html_nodes('.time_stamp') %>% html_text())
snapShotDate <- (html %>% html_nodes('.svn_info') %>% html_text())[1]
HeaderInfo <- (html %>% html_nodes('h1') %>% html_text())

timeStamp <- str_extract(timeStamp, "\\d{4}-\\d{2}-\\d{2}")
snapShotDate <- str_extract(snapShotDate, "\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}")
BiocVersion <- str_extract(HeaderInfo, "\\d+\\.\\d+")


# info needed
# Package, Stage, Status, Date, RepoType, Builder OS, Rversion/patch, BiocVersion
