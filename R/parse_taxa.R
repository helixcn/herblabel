parse_taxa <- function(taxa){
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
        names(res) <- c("TAXON_PARSED"                        ,
                        "GENUS_PARSED"                        ,
                        "SPECIES_PARSED"                      ,
                        "AUTHOR_OF_SPECIES_PARSED"            ,
                        "INFRASPECIFIC_RANK_PARSED"           ,
                        "INFRASPECIFIC_EPITHET_PARSED"        ,
                        "AUTHOR_OF_INFRASPECIFIC_RANK_PARSED" )
        return(res)
    }
    res <- data.frame(t(sapply(taxa, parse_taxon, USE.NAMES = FALSE)), stringsAsFactors = FALSE)
    return(res)
}
