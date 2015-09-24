fill_sp_dwc <- function(dat){

    parse_taxon <- function(taxon){
        replace_space <- function(x) {
                gsub("^[[:space:]]+|[[:space:]]+$", "", x)
        }
    
        if(length(taxon) > 1){
            stop("Only one taxon allowed")
        }
        
        GENUS                         <- ""
        SPECIES                       <- ""
        AUTHOR_OF_SPECIES             <- ""
        INFRASPECIFIC_RANK            <- ""
        INFRASPECIFIC_EPITHET         <- ""
        AUTHOR_OF_INFRASPECIFIC_RANK  <- ""    
        
        taxon <- gsub(" +", " ", replace_space(taxon))
        ### Parse Genus
        gap1 <- regexpr(pattern = " ", text = taxon)
        GENUS <- replace_space(substr(taxon, start = 1, stop = (gap1-1)))
        ### The rest part
        part1 <- replace_space(substr(taxon, start = gap1 + 1, stop = nchar(taxon)))
        gap2 <- regexpr(pattern = " ", text = part1)
            
        ### Parse Species
        if(gap2 < 0){
            SPECIES <- replace_space(substr(part1, start = gap2 + 1, stop = nchar(part1)))
            author_temp <- ""
        } else {
            SPECIES <- replace_space(substr(part1, start = 1, stop = (gap2 - 1)))
            author_temp <- replace_space(substr(part1, start = gap2 + 1, stop = nchar(part1)))
        }
        
        ### Parse Author
        if(!grepl("var.|subsp.", author_temp)){
            AUTHOR_OF_SPECIES <- author_temp
        } else {
            gap3 <- regexpr(pattern = " ", text = author_temp)
            
            if(grepl("var.", author_temp)){
                INFRASPECIFIC_RANK            <- "var."
                gap_var_or_subsp <- regexpr(pattern = "var.", text = author_temp) + nchar("var.")
                AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, start = 1, stop = gap_var_or_subsp - nchar("var.") -1))
            } else {
                if(grepl("subsp.", author_temp)){
                      INFRASPECIFIC_RANK            <- "subsp."
                      gap_var_or_subsp <- regexpr(pattern = "subsp.", text = author_temp) + nchar("subsp.")
                      AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, start = 1, stop = gap_var_or_subsp - nchar("subsp.") -1))
                    }
            }
            
            part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, start = gap_var_or_subsp + 1, stop = nchar(author_temp)))
            gap4 <- regexpr(pattern = " ", text = part_INFRASP_EP_AUTHOR_OF_INFRASP)
            if(gap4 > 0){
                INFRASPECIFIC_EPITHET         <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, start = 1, stop = gap4 - 1 ))
                AUTHOR_OF_INFRASPECIFIC_RANK  <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, start = gap4 + 1, stop = nchar(part_INFRASP_EP_AUTHOR_OF_INFRASP)))
            } else {
                INFRASPECIFIC_EPITHET         <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, start = 1, stop = nchar(part_INFRASP_EP_AUTHOR_OF_INFRASP)))
            }
        }
        
        if(!grepl(" ", taxon)){
           GENUS = taxon
           SPECIES                       <- ""
           AUTHOR_OF_SPECIES             <- ""
           INFRASPECIFIC_RANK            <- ""
           INFRASPECIFIC_EPITHET         <- ""
           AUTHOR_OF_INFRASPECIFIC_RANK  <- ""    
        }
        
        res <- c(taxon                        ,
                 GENUS                        ,
                 SPECIES                      ,
                 AUTHOR_OF_SPECIES            ,
                 INFRASPECIFIC_RANK           ,
                 INFRASPECIFIC_EPITHET        ,
                 AUTHOR_OF_INFRASPECIFIC_RANK )
        names(res) <- c("TAXON"                        ,
                        "GENUS"                        ,
                        "SPECIES"                      ,
                        "AUTHOR_OF_SPECIES"            ,
                        "INFRASPECIFIC_RANK"           ,
                        "INFRASPECIFIC_EPITHET"        ,
                        "AUTHOR_OF_INFRASPECIFIC_RANK" )
        return(res)
    } 

    parse_taxa <- function(taxa){
        res <- data.frame(t(sapply(taxa, parse_taxon)), stringsAsFactors = FALSE)
        return(res)    
    }

    families <- read.csv(system.file("extdata", "APGIII_GENERA.csv", package = "herblabel"), header = TRUE, stringsAsFactors = FALSE)
    spcn <- read.csv(system.file("extdata", "species_chinese_names.csv", package = "herblabel"), header = TRUE, stringsAsFactors = FALSE)
    
    #### add scientific name based on the local Chinese Name
    datspcn <- merge(dat, spcn, by.x = "LOCAL_NAME", by.y = "NAME_CN", sort = FALSE, all.x = TRUE)
    datspcn$SCIENTIFIC_NAME <- ifelse(is.na(datspcn$SCIENTIFIC_NAME)&!is.na(datspcn$LOCAL_NAME), datspcn$LOCAL_NAME, datspcn$SCIENTIFIC_NAME)
    datspcn$SCIENTIFIC_NAME <- ifelse(is.na(datspcn$SCIENTIFIC_NAME), "NOT_PROVIDED", datspcn$SCIENTIFIC_NAME)
    #### 
    datspcn <- datspcn[order(datspcn$SCIENTIFIC_NAME),]  
    ### add family
    res_parse0 <- parse_taxa(as.character(datspcn$SCIENTIFIC_NAME))
    res_parse <- merge(res_parse0, families, by = "GENUS", all.x = TRUE, sort = FALSE)
    res_parse <- res_parse[order(res_parse$TAXON),]
    
    if(!all(datspcn$SCIENTIFIC_NAME == res_parse$TAXON)){
       stop("Multiple families assigned for one genus, stop")
    } else {
       datspcn$FAMILY <- ifelse(is.na(datspcn$FAMILY), res_parse$FAMILY, datspcn$FAMILY  )
       datspcn$GENUS <- ifelse(is.na(datspcn$GENUS), res_parse$GENUS, datspcn$GENUS   )
       datspcn$SPECIES  <- ifelse(is.na(datspcn$SPECIES), res_parse$SPECIES,  datspcn$SPECIES )
       datspcn$AUTHOR_OF_SPECIES  <- ifelse(is.na(datspcn$AUTHOR_OF_SPECIES), res_parse$AUTHOR_OF_SPECIES,  datspcn$AUTHOR_OF_SPECIES )
       datspcn$INFRASPECIFIC_RANK  <- ifelse(is.na(datspcn$INFRASPECIFIC_RANK), res_parse$INFRASPECIFIC_RANK,  datspcn$INFRASPECIFIC_RANK )
       datspcn$INFRASPECIFIC_EPITHET  <- ifelse(is.na(datspcn$INFRASPECIFIC_EPITHET), res_parse$INFRASPECIFIC_EPITHET,  datspcn$INFRASPECIFIC_EPITHET )
       datspcn$AUTHOR_OF_INFRASPECIFIC_RANK  <- ifelse(is.na(datspcn$AUTHOR_OF_INFRASPECIFIC_RANK), res_parse$AUTHOR_OF_INFRASPECIFIC_RANK, datspcn$AUTHOR_OF_INFRASPECIFIC_RANK )
    }
    return(datspcn)
}

