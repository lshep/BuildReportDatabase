.libPaths("/home/lori/R/x86_64-pc-linux-gnu-library/4.0-BioC-3.12")

library(tidyr)
library(dplyr)
library(jsonlite)

versions <- c("3.11", "3.12")
 

for(ver in versions){

    #  ver = versions[1]

    
    tbl <- read.table(paste0("https://master.bioconductor.org/checkResults/",ver,"/bioc-LATEST/STATUS_DB.txt"), comment.char="")
    names(tbl) = c("Builder", "Status")
    
    res <- tbl %>% separate(Builder, c("Package", "Builder", "Stage"), "#")
    sta <- rep(0, dim(res)[1])
    sta[res[,"Status"] %in% c("ERROR", "TIMEOUT")] <- 1
    res[,"Status"] = sta
    
    active_builders <- unique(res[,"Builder"])
    package_list <- unique(res[,"Package"])

    DF <- data.frame(Package=package_list)
    DF[,active_builders] <- 0

    for(i in seq(1, dim(res)[1])){
        
        rowdx <- which(DF[,"Package"] == res[i,"Package"])
        coldx <- which(colnames(DF) == res[i, "Builder"])
        DF[rowdx, coldx] <-  DF[rowdx, coldx] + res[i, "Status"]        
    }
    
    for(dx in 2:dim(DF)[2]){
        DF[which(DF[,dx]>0),dx] <- 1       
    }

    # NOT SUPPORTED does not appear in tbl at all
    # Need placeholder for NOT SUPPORTED
    buildercounts <- as.data.frame(table(res[,c("Package","Builder")]),stringsAsFactors = FALSE )
    placeholders = buildercounts[which(buildercounts$Freq == 0),c("Package", "Builder")]
    for(dx in 1:dim(placeholders)[1]){
        DF[which(DF[,"Package"] == placeholders[dx,"Package"]),
           which(colnames(DF) == placeholders[dx, "Builder"])] = 1
    }

    # In case only one builder is active -- not likely but hey you never know
    if (length(active_builders) == 1){
        acrossAll <- DF[,2] > 0
        acrossAllCount <- as.numeric(acrossAll)
        anyFailure <- DF[,2] > 0
    } else {
        acrossAll <- rowSums(DF[,2:dim(DF)[2]]) == length(active_builders)
        acrossAllCount <- as.numeric(acrossAll)
        anyFailure <-  rowSums(DF[,2:dim(DF)[2]]) > 0
    }
    
    DF <- cbind(DF, acrossAllCount, ScriptRunCount=1, anyFailureToday=anyFailure, acrossAllToday=acrossAll)

    # Need cross check with manifest for BAD DESCRIPTION which don't appear
    # build results table    
    pkg_json <- read_json(paste0("https://master.bioconductor.org/packages/json/",ver,"/bioc/packages.json"))
    missing_pkgs <- names(pkg_json)[!(names(pkg_json) %in% DF[,"Package"])]
    len <- length(missing_pkgs)
    if(len > 0){
        addTo <- cbind(data.frame(Package=missing_pkgs,stringsAsFactors = FALSE),
                       matrix(1, nrow=len,ncol=length(active_builders)),
                       data.frame(acrossAllCounts=rep(1, len),
                                  ScriptRunCount=rep(1, len),
                                  anyFailureToday=rep(TRUE, len),
                                  acrossAllToday=rep(TRUE,len)))
        colnames(addTo) <- c("Package", active_builders, "acrossAllCount",
                             "ScriptRunCount", "anyFailureToday", "acrossAllToday")

        DF = rbind(DF, addTo)
        DF = arrange(DF, Package)
    }

    # first run only to have starting file
    # write.table(DF, paste0("SoftwarePkgSummary_",sub(ver, pattern=".", replacement="_",fixed=TRUE), ".csv"), row.names=FALSE, sep=",", quote=FALSE)
    
    OldTbl <- read.csv(paste0("SoftwarePkgSummary_",sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), header=TRUE)

    rmdx <- which(!(DF[,"Package"] %in% OldTbl[,"Package"]))
    if(length(rmdx) > 0){
        NewOnReport <- DF[rmdx,]
        DF = DF[(-rmdx),]
    }
    # Once appeared on report but now missing -- don't care leave in master
    # report
    # This shouldn't happen unless it was removed mid-release
    # OldMissingFromReport <- OldTbl[!(OldTbl[,"Package"] %in% DF[,"Package"]),] 

    MasterTbl <- OldTbl
    
    matchdx <- match(DF[,"Package"], MasterTbl[,"Package"])
    for(bld in active_builders){
        MasterTbl[matchdx,bld] = MasterTbl[matchdx,bld] + DF[,bld]
    }
    MasterTbl[matchdx,"acrossAllCount"] = MasterTbl[matchdx,"acrossAllCount"] + DF[,"acrossAllCount"]
    MasterTbl[matchdx,"ScriptRunCount"] = MasterTbl[matchdx,"ScriptRunCount"] + DF[,"ScriptRunCount"]
    MasterTbl[matchdx,"anyFailureToday"] = DF[,"anyFailureToday"]
    MasterTbl[matchdx,"acrossAllToday"] = DF[,"acrossAllToday"]

    if(length(rmdx) >0){
        MasterTbl <- rbind(MasterTbl, NewOnReport)
        MasterTbl <- arrange(MasterTbl, Package)
    }
    write.table(MasterTbl, paste0("SoftwarePkgSummary_",sub(ver, pattern=".", replacement="_",fixed=TRUE), ".csv"), row.names=FALSE, sep=",", quote=FALSE)    
    

    ##################################################
    #
    # Make Date spreadsheets
    #
    ##################################################

    # 
    # All
    #
    colDF <- data.frame(pkgs = MasterTbl[MasterTbl[,"acrossAllToday"],"Package"], date=1, stringsAsFactors = FALSE)
    colnames(colDF)[2] = format(Sys.time(), "%b%d")
    # first run only to have starting file
    #write.table(colDF, file = paste0("SoftwareAcrossAll_",sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), quote=FALSE, sep=",", row.names=FALSE)
    
    r <- read.table(file = paste0("SoftwareAcrossAll_",sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), sep=",", header=TRUE, stringsAsFactors=FALSE)
    idx = match(setdiff(as.character(colDF$pkgs), as.character(r$pkgs)), as.character(colDF$pkgs))
    if(length(idx)!= 0){
        subAdd = colDF[idx,]

        placeholder = dim(r)[2]-1
        for(i in 1:dim(subAdd)[1]){
            r = rbind(r, c(subAdd[i,1], rep(0, placeholder)))
        }
    }
    tempVec = colDF[match(as.character(r$pkgs),  as.character(colDF$pkgs)),2]
    tempVec[is.na(tempVec)] = 0
    r = cbind(r, newCol = tempVec)
    colnames(r)[dim(r)[2]] = format(Sys.time(), "%b%d")

    write.table(r, file = paste0("SoftwareAcrossAll_",sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), quote=FALSE, sep=",", row.names=FALSE)
    
    #
    # Per builder
    #
    for(ab in active_builders){
        colDF <- data.frame(pkgs = MasterTbl[as.logical(MasterTbl[,ab]),"Package"], date=1, stringsAsFactors = FALSE)
        colnames(colDF)[2] = format(Sys.time(), "%b%d")
        # first run only to have starting file
        #write.table(colDF, file = paste0("Software_",ab,"_", sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), quote=FALSE, sep=",", row.names=FALSE)
        
        r <- read.table(file = paste0("Software_",ab,"_", sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), sep=",", header=TRUE, stringsAsFactors=FALSE)
        idx = match(setdiff(as.character(colDF$pkgs), as.character(r$pkgs)), as.character(colDF$pkgs))
        if(length(idx)!= 0){
            subAdd = colDF[idx,]
            
            placeholder = dim(r)[2]-1
            for(i in 1:dim(subAdd)[1]){
                r = rbind(r, c(subAdd[i,1], rep(0, placeholder)))
            }
        }
        tempVec = colDF[match(as.character(r$pkgs),  as.character(colDF$pkgs)),2]
        tempVec[is.na(tempVec)] = 0
        r = cbind(r, newCol = tempVec)
        colnames(r)[dim(r)[2]] = format(Sys.time(), "%b%d")
        write.table(r, file = paste0("Software_",ab,"_", sub(ver, pattern=".", replacement="_",fixed=TRUE),".csv"), quote=FALSE, sep=",", row.names=FALSE)

    }    
}

#
# A Report for failing on release and devel across all 
#

if (length(versions) == 2){

    builder1 =  read.table(file =
        paste0("SoftwareAcrossAll_",sub(versions[1], pattern=".",
                                                 replacement="_",fixed=TRUE),".csv"),
        sep=",", header=TRUE, stringsAsFactors=FALSE)

    builder2 =  read.table(file =
        paste0("SoftwareAcrossAll_",sub(versions[2], pattern=".",
                                                 replacement="_",fixed=TRUE),".csv"),
        sep=",", header=TRUE, stringsAsFactors=FALSE)

    Pkgs1 = builder1[which(builder1[,dim(builder1)[2]] > 0),"pkgs"]
    Pkgs2 = builder2[which(builder2[,dim(builder2)[2]] > 0),"pkgs"]

    Both = intersect(Pkgs1, Pkgs2)
    colDF <- data.frame(pkgs = sort(Both), date=1, stringsAsFactors = FALSE)
    colnames(colDF)[2] = format(Sys.time(), "%b%d")
    # first run only to have starting file
    #write.table(colDF, file = "Software_BothReleases.csv", quote=FALSE, sep=",", row.names=FALSE)
    
    r <- read.table(file = "Software_BothReleases.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
    idx = match(setdiff(as.character(colDF$pkgs), as.character(r$pkgs)), as.character(colDF$pkgs))
    if(length(idx)!= 0){
        subAdd = colDF[idx,]
        
        placeholder = dim(r)[2]-1
        for(i in 1:dim(subAdd)[1]){
            r = rbind(r, c(subAdd[i,1], rep(0, placeholder)))
        }
    }
    tempVec = colDF[match(as.character(r$pkgs),  as.character(colDF$pkgs)),2]
    tempVec[is.na(tempVec)] = 0
    r = cbind(r, newCol = tempVec)
    colnames(r)[dim(r)[2]] = format(Sys.time(), "%b%d")
    write.table(r, file = "Software_BothReleases.csv", quote=FALSE, sep=",", row.names=FALSE)

    
}
