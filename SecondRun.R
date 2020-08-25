library(tidyr)
library(dplyr)
library(jsonlite)
library(yaml)
library(httr)
library(RMariaDB)
library(DBI)
library(dplyr)
library(plyr)


config =  read_yaml("https://master.bioconductor.org/config.yaml")


versions <- c(config$release_version, config$devel_version)

con <- dbConnect(RMariaDB::MariaDB(), group = "my-db")
 

#ver = versions[1]

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

         # See if failed to build report based on date
         qry <- paste0("SELECT * FROM reports WHERE date='",date_report,"';")
         res <- dbSendQuery(con, qry)
         tbl_reports<- dbFetch(res)
         dbClearResult(res)
  
         if(nrow(tbl_reports) != 0){
             message("Duplicate Report. Build Report Failed to post")
             next
         }else{
             repo_type <- ifelse(repo == "bioc","software", repo)
             dbAppendTable(con, "reports", data.frame(date=date_report, repo_type=repo_type))
             
             qry <- paste0("SELECT * FROM reports WHERE date='",date_report,"';")
             res <- dbSendQuery(con, qry)
             tbl_reports<- dbFetch(res)
             dbClearResult(res)
         }

    
###############################################
         
# Read Status file

###############################################
         tbl <- read.table(file, comment.char="")
         
         names(tbl) = c("builder", "status")
         status <- tbl %>% separate(builder, c("package", "builder", "stage"), "#")

         # git problems as defined by skipped, ERROR, TIMEOUT
         idx <- which(tolower(status[,"status"]) %in% tolower(c("skipped","ERROR",
                                                                "TIMEOUT")))

         status <- status[idx,]
         
         gitcommitid <- rep("", dim(status)[1])
         gitcommitdate <- rep("", dim(status)[1])
         
         
         for(i in seq_len(dim(status)[1])){

             pkg <- status[i, "package"]
             dcf <- read.dcf(paste0("/home/shepherd/Projects/BuildReportDatabase/TempCopyOfFiles/bioc/gitlog/git-log-", pkg,".dcf"))
             gitcommitid[i] <- dcf[,"Last Commit"]
             gitcommitdate[i] <- dcf[,"Last Changed Date"]
         }


         pkgs <- unique(status[,"package"])
         for(pkg in pkgs){
             
             dcf <- read.dcf(paste0("/home/shepherd/Projects/BuildReportDatabase/TempCopyOfFiles/bioc/gitlog/git-log-", pkg,".dcf"))
             gitcommitid[which(status[,"package"]==pkg)] <- dcf[,"Last Commit"]
             gitcommitdate[which(status[,"package"]==pkg)] <- dcf[,"Last Changed Date"]
             
         }


         status <- cbind(status, git_commit_id=gitcommitid, git_commit_date=gitcommitdate)


###############################################
         
# Check today's builders
#    If needed add to builders database table
#    Else retrieve builder_id

###############################################

         
         ActiveBuilders <- system2("ls", args= "/home/shepherd/Projects/BuildReportDatabase/TempCopyOfFiles/bioc/nodes", stdout=TRUE)
         df <- matrix("", nrow=length(ActiveBuilders), ncol=4)
         rownames(df) <- ActiveBuilders
         colnames(df) <- c("r_version", "platform", "os", "bioc_version")
         
         for(i in ActiveBuilders){
             text <-
                 readLines(paste0("/home/shepherd/Projects/BuildReportDatabase/TempCopyOfFiles/bioc/nodes/",i,"/NodeInfo/R-sessionInfo.txt"),
                           n=3)
             df[i,] <-  c(trimws(gsub(pattern="Platform:|Running under:", replacement="", text)), ver)
             
         }

    
         res <- dbSendQuery(con, "SELECT * FROM builders")
         builders <- dbFetch(res)
         dbClearResult(res)


         # verify there is an entry in the database and get builder_id for df 
         builder_id <- rep(NA_integer_, nrow(df))
         found <- match_df(builders, as.data.frame(df))
         builder_id[match(unname(unlist(found["builder"])),  rownames(df))] = found$builder_id
         

         # Update builders table if needed
         if (nrow(df) != nrow(found)){
             
             not_fnd <- df[-(match(found["builder"], rownames(df))),]
             not_fnd <- cbind(not_fnd, builder=rownames(not_fnd))
             not_fnd <- as.data.frame(not_fnd) %>% select(colnames(builders)[-1])
             dbAppendTable(con, "builders", not_fnd)    
             
             res <- dbSendQuery(con, "SELECT * FROM builders")
             builders <- dbFetch(res)
             dbClearResult(res)
             
             builder_id <- rep(NA_integer_, nrow(df))
             found <- match_df(builders, as.data.frame(df))
             builder_id[match(unname(unlist(found["builder"])),  rownames(df))] = found$builder_id
             
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
