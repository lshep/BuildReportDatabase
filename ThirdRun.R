######################################################################################
#
#On malbec2 in ~biocbuild/public_html/BBS/3.11/bioc:
#
#   - meat-index.dcf: same list of packages as in the manifest EXCEPT
#that packages that have a DESCRIPTION file that is too broken for the
#builds are separated and put in skipped-index.dcf
#
#   - skipped-index.dcf: list of packages from manifest that have a
#DESCRIPTION file that is too broken for the builds.
#
#   - gitlog: folder containing 1 little dcf file per package in the
#manifest e.g. gitlog/git-log-BiocGenerics.dcf:
#
#
#    The OS, Platform, and R version used on each node can be extracted from
#~biocbuild/public_html/BBS/3.11/bioc/nodes/<node_name>/NodeInfo/R-sessionInfo.txt

if(F){

cd /home/shepherd/Projects/ReportDatabase/BuildReportDatabase/TempCopyOfFiles

scp biocbuild@malbec1.bioconductor.org:/home/biocbuild/public_html/BBS/3.12/bioc/*.dcf 3.12/bioc/
scp -r biocbuild@malbec1.bioconductor.org:/home/biocbuild/public_html/BBS/3.12/bioc/gitlog 3.12/bioc/
scp -r biocbuild@malbec1.bioconductor.org:/home/biocbuild/public_html/BBS/3.12/bioc/nodes/malbec1/NodeInfo 3.12/bioc/nodes/malbec1/
scp -r biocbuild@malbec1.bioconductor.org:/home/biocbuild/public_html/BBS/3.12/bioc/nodes/tokay1/NodeInfo 3.12/bioc/nodes/tokay1/
scp -r biocbuild@malbec1.bioconductor.org:/home/biocbuild/public_html/BBS/3.12/bioc/nodes/merida1/NodeInfo 3.12/bioc/nodes/merida1/
scp -r biocbuild@malbec1.bioconductor.org:/home/biocbuild/public_html/BBS/3.12/bioc/nodes/nebbiolo1/NodeInfo 3.12/bioc/nodes/nebbiolo1/


scp biocbuild@malbec2.bioconductor.org:/home/biocbuild/public_html/BBS/3.13/bioc/*.dcf 3.13/bioc/
scp -r biocbuild@malbec2.bioconductor.org:/home/biocbuild/public_html/BBS/3.13/bioc/gitlog 3.13/bioc/
scp -r biocbuild@malbec2.bioconductor.org:/home/biocbuild/public_html/BBS/3.13/bioc/nodes/malbec2/NodeInfo 3.13/bioc/nodes/malbec2/
scp -r biocbuild@malbec2.bioconductor.org:/home/biocbuild/public_html/BBS/3.13/bioc/nodes/tokay2/NodeInfo 3.13/bioc/nodes/tokay2/
scp -r biocbuild@malbec2.bioconductor.org:/home/biocbuild/public_html/BBS/3.13/bioc/nodes/machv2/NodeInfo 3.13/bioc/nodes/machv2/

}
    
######################################################################################

library(tidyr)
library(dplyr)
library(jsonlite)
library(yaml)
library(httr)
library(RMariaDB)
library(DBI)
library(dplyr)
library(plyr)
library(stringr)


config =  read_yaml("https://master.bioconductor.org/config.yaml")


versions <- c(config$release_version, config$devel_version)

# See the following for mariadb setup
# https://github.com/r-dbi/RMariaDB
con <- dbConnect(RMariaDB::MariaDB(), group = "my-db")
 

##  ver = versions[1]

for(ver in versions){
    message("working on release: ", ver) 
    
  # eventually loop over repos but for now concentrate on one
    repos <- c("bioc", "data-experiment", "workflows") #,"data-annotation")
    # for(repo in repos) {
    repo="bioc"
    message("working on repo: ", repo) 
    
    
    #repo = repos[1]

         file <- paste0("https://master.bioconductor.org/checkResults/",ver,"/",
                        repo, "-LATEST/STATUS_DB.txt")
         
###############################################
         
# Check report date
#    If found, the build report didn't generate
#    skip to not duplicate entries

###############################################
         date_report <- cache_info(HEAD(file))[["modified"]]
         date_formatted <- format(date_report,"%Y-%m-%d")
    
         tbl_reports <- tbl(con, "reports") %>% filter(str_detect(date, date_formatted)) %>% collect(Inf)

         if(nrow(tbl_reports) != 0){
             message("Duplicate Report. Build Report Failed to post")
             next
         }else{
             repo_type <- ifelse(repo == "bioc","software", repo)
             dbAppendTable(con, "reports", data.frame(date=date_report, repo_type=repo_type))
             
             tbl_reports <- tbl(con, "reports") %>% filter(str_detect(date, date_formatted)) %>% collect(Inf)
         }

    
###############################################
         
# Read Status file

###############################################
         tbl <- read.table(file, comment.char="")
         
         names(tbl) = c("builder", "status")
         status <- tbl %>% separate(builder, c("package", "builder", "stage"), "#")
         status$stage <- gsub(status$stage, pattern=":", replacement="")
         status$status[which(is.na(status$status))] = "NA"
         # git problems as defined by skipped, ERROR, TIMEOUT
         #idx <- which(tolower(status[,"status"]) %in% tolower(c("skipped","ERROR",
         #                                                       "TIMEOUT")))

         #status <- status[idx,]
         
         gitcommitid <- rep("", dim(status)[1])
         gitcommitdate <- rep("", dim(status)[1])
         
         
         for(i in seq_len(dim(status)[1])){

             pkg <- status[i, "package"]
             dcf <-
             read.dcf(paste0("/home/shepherd/Projects/ReportDatabase/BuildReportDatabase/TempCopyOfFiles/", ver, "/bioc/gitlog/git-log-", pkg,".dcf"))
             gitcommitid[i] <- dcf[,"Last Commit"]
             gitcommitdate[i] <- dcf[,"Last Changed Date"]
         }

## Is this faster?
##         pkgs <- unique(status[,"package"])
##         for(pkg in pkgs){
##             
##             dcf <-
##             read.dcf(paste0("/home/shepherd/Projects/BuildReportDatabase/TempCopyOfFiles/", ver, "/bioc/gitlog/git-log-", pkg,".dcf"))
##             gitcommitid[which(status[,"package"]==pkg)] <- dcf[,"Last Commit"]
##             gitcommitdate[which(status[,"package"]==pkg)] <- dcf[,"Last Changed Date"]
##             
##         }


         status <- cbind(status, git_commit_id=gitcommitid, git_commit_date=gitcommitdate)


###############################################
         
# Check today's builders
#    If needed add to builders database table
#    Else retrieve builder_id

###############################################


         ActiveBuilders <- system2("ls", args= paste0("/home/shepherd/Projects/ReportDatabase/BuildReportDatabase/TempCopyOfFiles/", ver, "/bioc/nodes"), stdout=TRUE)
         df <- matrix("", nrow=length(ActiveBuilders), ncol=4)
         rownames(df) <- ActiveBuilders
         colnames(df) <- c("r_version", "platform", "os", "bioc_version")
         
         for(i in ActiveBuilders){
             text <-
                 readLines(paste0("/home/shepherd/Projects/ReportDatabase/BuildReportDatabase/TempCopyOfFiles/", ver, "/bioc/nodes/",i,"/NodeInfo/R-sessionInfo.txt"),
                           n=3)
             df[i,] <-  c(trimws(gsub(pattern="Platform:|Running under:", replacement="", text)), ver)
             
         }

    
         builders <-  tbl(con, "builders") %>% collect(Inf)
    

         # verify there is an entry in the database and get builder_id for df 
         builder_id <- rep(NA_integer_, nrow(df))
         found <- match_df(builders, as.data.frame(df))
         builder_id[match(unname(unlist(found["builder"])),  rownames(df))] = found$builder_id
         

         # Update builders table if needed
         if (nrow(df) != nrow(found)){
             
             if(nrow(found) == 0){
                 not_fnd <- cbind(as.data.frame(df), builder=rownames(df))
             }else{              
                 not_fnd <- df[-(match(found$builder, rownames(df))),,drop=FALSE]
                 not_fnd <- cbind(not_fnd, builder=rownames(not_fnd))
                 not_fnd <- as.data.frame(not_fnd) %>% select(colnames(builders)[-1])
             }
             dbAppendTable(con, "builders", not_fnd)    
             
             builders <-  tbl(con, "builders") %>% collect(Inf)
    
             builder_id <- rep(NA_integer_, nrow(df))
             found <- match_df(builders, as.data.frame(df))
             builder_id[match(unname(unlist(found["builder"])),  rownames(df))] = found$builder_id
             
         } else {
             message("All builders found")
         }

         df <- cbind(builder_id, df)

         
###############################################
         
# Update status table for
#    builder_id and report_id
# Add to database
         
###############################################

         status$builder_id= unname(df[match(status$builder, rownames(df)), "builder_id"])
         status$report_id = unname(tbl_reports$report_id)

         status <- select(status, c("builder_id", "report_id", "package",
                                    "git_commit_id", "git_commit_date", "stage", "status")) 
         dbAppendTable(con, "status", status)


         # need to check skipped file for errors to add to status


     #} #End loop over repositories
    
} # End loop over versions


    
#Disconnect from the database
dbDisconnect(con)



###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################






## biocBuildReport package

library(RMariaDB)
library(dplyr)
library(stringr)

con <- dbConnect(RMariaDB::MariaDB(), group = "my-db")

## bu = tbl(con, "builders")
## rt = tbl(con, "reports")
## st = tbl(con, "status")

## ov = tbl(con, "overview")

## Disconnect from the database
## dbDisconnect(con)


library(dbplyr)





##########################################################
##
## Is my package failing on today's build report?
##
###########################################################

today <- format(Sys.time(), "%Y-%m-%d")

todays_report_id <- tbl(con, "reports") %>% filter(str_detect(date, today)) %>%
    select("report_id") %>% collect(Inf) %>% `[[`("report_id")

if(length(todays_report_id) == 0){
    message("Build Report For Today is not posted yet")
}


if(F){
    
    yesterday <- format(Sys.time()-(24*60*60), "%Y-%m-%d")
    todays_report_id <- tbl(con, "reports") %>% filter(str_detect(date, yesterday)) %>%
        select("report_id") %>% collect(Inf) %>% `[[`("report_id")
}



#pkg = "BiocFileCache"
pkg = "a4"

any((tbl(con, "status") %>% filter(report_id %in% todays_report_id) %>%
 filter(package == pkg) %>% collect %>% `[[`("status") %>% unique) %in%
    c("ERROR", "TIMEOUT"))



## In release or devel? 
temp <- inner_join((tbl(con, "status") %>% filter(report_id %in% todays_report_id) %>%
                    filter(package == pkg) %>% filter(status %in% c("ERROR", "TIMEOUT"))),
                   tbl(con, "builders"))
temp %>% collect %>% `[[`("bioc_version") %>% unique()


## On what builders?
temp %>% collect %>% `[[`("builder") %>% unique()




##########################################################
##
## How long has my package been failing?
##
###########################################################

pkg = "a4"

temp = tbl(con, "status") %>% filter(package == pkg) %>% select("builder_id",
                                                         "report_id", "status")

temp2 = inner_join(inner_join((temp %>% filter(status %in%  c("ERROR", "TIMEOUT"))),
                      tbl(con, "reports")), tbl(con, "builders")) %>%
                      select("status", "date", "builder", "bioc_version")

temp2 %>% arrange(date)

##########################################################
##
## What commit version is the builder using?
##
###########################################################


today <- format(Sys.time(), "%Y-%m-%d")
todays_report_id <- tbl(con, "reports") %>% filter(str_detect(date, today)) %>% select("report_id") %>% collect(Inf) %>% `[[`("report_id") 


if(length(todays_report_id) == 0){
    message("Build Report For Today is not posted yet")
}


if(F){
    
    yesterday <- format(Sys.time()-(24*60*60), "%Y-%m-%d")
    todays_report_id <- tbl(con, "reports") %>% filter(str_detect(date, yesterday)) %>%
        select("report_id") %>% collect(Inf) %>% `[[`("report_id")
}



#pkg = "AnnotationHub"
pkg = "a4"

## tbl(con, "status") %>% filter(report_id %in% todays_report_id) %>% filter(package == pkg) %>% select("git_commit_id", "git_commit_date") %>% distinct()
## potentially different
temp <- inner_join((tbl(con, "status") %>% filter(report_id %in% todays_report_id) %>%
                    filter(package == pkg)),
                   tbl(con, "builders"))
temp %>% select("builder", "bioc_version", "git_commit_id", "git_commit_date") %>% distinct()





if(F){
    
## think it would be useful to ask historical questions, like 'how many successes has my package had, across all platforms, in the last x days'. Also it might be useful to ask more complicated questions, like 'for each day, did my package build successfully? did any of it's dependencies fail to build? where there changes (git commit ids?) in any of my dependencies' and also 'when I changed my package, did any of my reverse dependencies break?' Most of these I'm thinking return a tibble with columns for date, my package status, dependency/reverse dependency status, etc
    
}


## Disconnect from database
dbDisconnect(con)

