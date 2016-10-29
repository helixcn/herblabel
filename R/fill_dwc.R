#### dat <- dat_test

fill_dwc <- function(dat){
    add.sort.id <- 1:nrow(dat)
    dat2 <- data.frame(dat, add.sort.id)
    syst <- Sys.info()[['sysname']]
    pgenus <- herblabel::pgenus
    spcn   <- herblabel::spcn
    #### add scientific name based on the local Chinese Name
    datspcn <- merge(dat2, spcn, by.x = "LOCAL_NAME", by.y = "NAME_CN", sort = FALSE, all.x = TRUE)
    datspcn$SCIENTIFIC_NAME <- ifelse((is.na(datspcn$SCIENTIFIC_NAME)|datspcn$SCIENTIFIC_NAME == "")&(!is.na(datspcn$LOCAL_NAME)|datspcn$LOCAL_NAME == ""), datspcn$LOCAL_NAME, datspcn$SCIENTIFIC_NAME)
    datspcn$SCIENTIFIC_NAME <- ifelse((is.na(datspcn$SCIENTIFIC_NAME)|datspcn$SCIENTIFIC_NAME == ""), "", datspcn$SCIENTIFIC_NAME)
    #### 
    ### add family
    res_parse0 <- parse_taxa(as.character(datspcn$SCIENTIFIC_NAME))
    datspcn2 <- cbind(datspcn, res_parse0)
    
    if(nrow(datspcn2) != nrow(datspcn)){
       warning("Multiple families assigned for one genus")
    }

    datspcn2$GENUS                         <- ifelse(is.na(datspcn2$GENUS)                       |datspcn2$GENUS                        == "", datspcn2$GENUS_PARSED,                        datspcn2$GENUS                        )
    datspcn2$SPECIES                       <- ifelse(is.na(datspcn2$SPECIES)                     |datspcn2$SPECIES                      == "", datspcn2$SPECIES_PARSED,                      datspcn2$SPECIES                      )
    datspcn2$AUTHOR_OF_SPECIES             <- ifelse(is.na(datspcn2$AUTHOR_OF_SPECIES)           |datspcn2$AUTHOR_OF_SPECIES            == "", datspcn2$AUTHOR_OF_SPECIES_PARSED,            datspcn2$AUTHOR_OF_SPECIES            )
    datspcn2$INFRASPECIFIC_RANK            <- ifelse(is.na(datspcn2$INFRASPECIFIC_RANK)          |datspcn2$INFRASPECIFIC_RANK           == "", datspcn2$INFRASPECIFIC_RANK_PARSED,           datspcn2$INFRASPECIFIC_RANK           )
    datspcn2$INFRASPECIFIC_EPITHET         <- ifelse(is.na(datspcn2$INFRASPECIFIC_EPITHET)       |datspcn2$INFRASPECIFIC_EPITHET        == "", datspcn2$INFRASPECIFIC_EPITHET_PARSED,        datspcn2$INFRASPECIFIC_EPITHET        )
    datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK  <- ifelse(is.na(datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK)|datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK == "", datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK_PARSED, datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK )
    
    datspcn2 <- merge(datspcn2, pgenus, by.x = "GENUS",by.y = "GENUS", all.x = TRUE, sort = FALSE)
    datspcn2$FAMILY                        <- ifelse(is.na(datspcn2$FAMILY.x)                    |datspcn2$FAMILY.x                     == "", datspcn2$FAMILY.y,                            datspcn2$FAMILY.x                     )
    
    datspcn2 <- datspcn2[order(datspcn2$add.sort.id), ]
    
    ##### If the data has been successfully filled, do not fill again. Avoid duplicate entries introduced by merge. 
    ##### datspcn2_unique <- unique(datspcn2)
    ##### res.test <- subset(datspcn2_unique, select = colnames(dat))
    res.test <- subset(datspcn2, select = colnames(dat))
    
    res.test_combine_tax <- paste(
        res.test$COLLECTOR,
        res.test$COLLECTOR_NUMBER,
        res.test$FAMILY,
        res.test$GENUS,
        res.test$SPECIES,
        res.test$AUTHOR_OF_SPECIES,
        res.test$INFRASPECIFIC_RANK,
        res.test$INFRASPECIFIC_EPITHET,
        res.test$AUTHOR_OF_INFRASPECIFIC_RANK, sep = "_")
    
    dat_combine_tax <- paste(
        dat$COLLECTOR,
        dat$COLLECTOR_NUMBER,
        dat$FAMILY,
        dat$GENUS,
        dat$SPECIES,
        dat$AUTHOR_OF_SPECIES,
        dat$INFRASPECIFIC_RANK,
        dat$INFRASPECIFIC_EPITHET,
        dat$AUTHOR_OF_INFRASPECIFIC_RANK, sep = "_")
    
    if(all(res.test_combine_tax %in% dat_combine_tax)){
        res <- dat 
    } else {
        res <- subset(datspcn2, select = colnames(dat)) 
    }
    return(res)
}
