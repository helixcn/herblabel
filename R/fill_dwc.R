fill_dwc <- function(dat, namedb = c("spfrps", "spfoc")){
    namedb <- match.arg(namedb)
    add.sort.id <- 1:nrow(dat)
    dat2 <- data.frame(dat, add.sort.id)
    syst <- Sys.info()[['sysname']]
    pgenus  <- herblabel::pgenus
    spfrps  <- herblabel::spfrps
    spfoc   <- herblabel::spfoc
    #### add scientific name based on the local Chinese Name
    if(namedb == "spfrps"){
        datspcn <- merge(dat2, spfrps, by.x = "LOCAL_NAME", by.y = "NAME_CN", sort = FALSE, all.x = TRUE)
    }
    
    if(namedb == "spfoc"){
        datspcn <- merge(dat2, spfoc,  by.x = "LOCAL_NAME", by.y = "NAME_CN", sort = FALSE, all.x = TRUE)
    }
    
    datspcn$SCIENTIFIC_NAME <- ifelse((is.na(datspcn$SCIENTIFIC_NAME)|datspcn$SCIENTIFIC_NAME == "")&(!is.na(datspcn$LOCAL_NAME)|datspcn$LOCAL_NAME == ""), datspcn$LOCAL_NAME, datspcn$SCIENTIFIC_NAME)
    datspcn$SCIENTIFIC_NAME <- ifelse((is.na(datspcn$SCIENTIFIC_NAME)|datspcn$SCIENTIFIC_NAME == ""), "", datspcn$SCIENTIFIC_NAME)
    #### 
    ### add family
    res_parse0 <- parse_taxa(as.character(datspcn$SCIENTIFIC_NAME))
    datspcn2 <- cbind(datspcn, res_parse0)
    
    if(nrow(datspcn2) != nrow(datspcn)){
       warning("Multiple families assigned for one genus")
    }

    ##### #### Clear the previous results
    ##### datspcn2$GENUS                              <- ""
    ##### datspcn2$SPECIES                            <- ""
    ##### datspcn2$AUTHOR_OF_SPECIES                  <- ""
    ##### datspcn2$INFRASPECIFIC_RANK                 <- ""
    ##### datspcn2$INFRASPECIFIC_EPITHET              <- ""
    ##### datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK       <- ""
    
    #### Make sure that no NA in the dataframe so all the cells could be compared.
    datspcn2[is.na(datspcn2)] <- ""
    
    #### If the original fields for the scientific name does not match with the one from the local name, replace with the results derived from the local name.
    for (i in 1:nrow(datspcn2)){
        if(!datspcn2$LOCAL_NAME[i] == ""){
            datspcn2$GENUS[i]                         <- ifelse(!datspcn2$GENUS_PARSED [i]                         == datspcn2$GENUS[i]                        , datspcn2$GENUS_PARSED[i],                        datspcn2$GENUS[i]                        )
            datspcn2$SPECIES[i]                       <- ifelse(!datspcn2$SPECIES_PARSED [i]                       == datspcn2$SPECIES [i]                     , datspcn2$SPECIES_PARSED[i],                      datspcn2$SPECIES [i]                     )
            datspcn2$AUTHOR_OF_SPECIES[i]             <- ifelse(!datspcn2$AUTHOR_OF_SPECIES_PARSED [i]             == datspcn2$AUTHOR_OF_SPECIES [i]           , datspcn2$AUTHOR_OF_SPECIES_PARSED[i],            datspcn2$AUTHOR_OF_SPECIES  [i]          )
            datspcn2$INFRASPECIFIC_RANK[i]            <- ifelse(!datspcn2$INFRASPECIFIC_RANK_PARSED [i]            == datspcn2$INFRASPECIFIC_RANK [i]          , datspcn2$INFRASPECIFIC_RANK_PARSED[i],           datspcn2$INFRASPECIFIC_RANK [i]          )
            datspcn2$INFRASPECIFIC_EPITHET[i]         <- ifelse(!datspcn2$INFRASPECIFIC_EPITHET_PARSED [i]         == datspcn2$INFRASPECIFIC_EPITHET [i]       , datspcn2$INFRASPECIFIC_EPITHET_PARSED[i],        datspcn2$INFRASPECIFIC_EPITHET [i]       )
            datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK[i]  <- ifelse(!datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK_PARSED [i]  == datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK [i], datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK_PARSED[i], datspcn2$AUTHOR_OF_INFRASPECIFIC_RANK [i])
        }
    }
    
    datspcn2 <- merge(datspcn2, pgenus, by.x = "GENUS", by.y = "GENUS", all.x = TRUE, sort = FALSE)
        
    #### Allow the family to be displayed even the local name is empty
    for (i in 1:nrow(datspcn2)){
        if(datspcn2$LOCAL_NAME[i] == ""){
            datspcn2$FAMILY[i] <- datspcn2$FAMILY.x[i]
        } else {
            datspcn2$FAMILY[i] <- datspcn2$FAMILY.y[i]
        }
    }
    
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
    
    res <- subset(datspcn2, select = colnames(dat)) 
    return(res)
}
