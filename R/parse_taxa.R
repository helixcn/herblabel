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
                AUTHOR_OF_SPECIES <- author_temp
            
                gap3 <- regexpr(pattern = " ", text = author_temp)
            if(grepl("var\\. ", taxon)|grepl("subsp\\. ", taxon)|grepl(" f\\. ", taxon)){
                if(grepl("var\\. ", taxon)){
                    INFRASPECIFIC_RANK            <- "var."
                    gap_var <- regexpr(pattern = "var\\. ", text = author_temp) + nchar("var.")
                    AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, start = 1, stop = gap_var - nchar("var.") -1))
                    part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, start = gap_var + 1, stop = nchar(author_temp)))
                } else {
                    if(grepl("subsp\\. ", taxon)){
                        INFRASPECIFIC_RANK            <- "subsp."
                        gap_subsp <- regexpr(pattern = "subsp\\. ", text = author_temp) + nchar("subsp.")
                        AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, start = 1, stop = gap_subsp - nchar("subsp.") -1))
                        part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, start = gap_subsp + 1, stop = nchar(author_temp)))
                    } else {
                        if(grepl(" f\\. ", taxon)){
                            position_f <- gregexpr(pattern = " f\\. ", text = taxon)
                            if(length(position_f[[1]]) > 1){warning("There are more than one \"f.\" found in the name, there may be errors in parsing the names.")}
                            for(i in 1:length(position_f[[1]])){
                                position_space <- gregexpr(pattern = " ", text = taxon)
                                space_position_f <- which(position_space[[1]] %in% position_f[[1]])
                                space_position_f_1 <- space_position_f - 1
                                word_before_f <- replace_space(substr(taxon, start = position_space[[1]][space_position_f_1], stop = position_space[[1]][space_position_f]))
                                if( replace_space(word_before_f) == SPECIES){
                                    INFRASPECIFIC_RANK            <- "f."
                                    gap_f <- regexpr(pattern = " f\\. ", text = author_temp) + nchar(" f. ")
                                    AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, start = 1, stop = gap_f - nchar(" f. ") -1))
                                    part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, start = gap_f + 1, stop = nchar(author_temp)))
                                }
                            }
                        }
                    }
                }
                ##part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, start = gap_var_or_subsp + 1, stop = nchar(author_temp)))
                 gap4 <- regexpr(pattern = " ", text = part_INFRASP_EP_AUTHOR_OF_INFRASP)
                 if(gap4 > 0){
                     INFRASPECIFIC_EPITHET         <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, start = 1, stop = gap4 - 1 ))
                     AUTHOR_OF_INFRASPECIFIC_RANK  <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, start = gap4 + 1, stop = nchar(part_INFRASP_EP_AUTHOR_OF_INFRASP)))
                 } else {
                     INFRASPECIFIC_EPITHET         <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, start = 1, stop = nchar(part_INFRASP_EP_AUTHOR_OF_INFRASP)))
                     if(INFRASPECIFIC_EPITHET %in% strsplit(AUTHOR_OF_SPECIES, " ")[[1]]){
                         INFRASPECIFIC_EPITHET <- ""
                     }
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
