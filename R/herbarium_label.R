#### create herbarium labels in RTF, default size of paper is A4.

herbarium_label <- function(dat = NULL, infile = NULL, spellcheck = TRUE, outfile = "herblabel.rtf"){
    if(is.null(dat)&is.null(infile)){
        stop("at least dat or infile should be specified")
    }
    if(!is.null(dat)&!is.null(infile)){
        stop("dat and infile should be not be specified together")
    }
    if(is.null(dat)){
        herbdat000 <- read.csv(infile, header = TRUE, stringsAsFactors = FALSE)
    } else {
        herbdat000 <- dat
    }
    if(any(is.na(herbdat000$HERBARIUM))){
        stop(paste("\"HERBARIUM\" must be provided for row: ", 
             paste(which(is.na(herbdat000$HERBARIUM))+1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COLLECTOR))){
        stop(paste("\"COLLECTOR\" must be provided for row: ", 
             paste(which(is.na(herbdat000$COLLECTOR))+1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COLLECTOR_NUMBER))){
        stop(paste("\"COLLECTOR_NUMBER\" must be provided for row: ", 
             paste(which(is.na(herbdat000$COLLECTOR_NUMBER)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$DATE_COLLECTED))){
        stop(paste("\"DATE_COLLECTED\" must be provided for row: ", 
             paste(which(is.na(herbdat000$DATE_COLLECTED)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$FAMILY))){
        stop(paste("\"FAMILY\" must be provided for row: ", 
             paste(which(is.na(herbdat000$FAMILY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$GENUS))){
        stop(paste("\"GENUS\" must be provided for row: ", 
             paste(which(is.na(herbdat000$GENUS)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COUNTRY))){
        stop(paste("\"COUNTRY\" must be provided for row: ", 
             paste(which(is.na(herbdat000$COUNTRY)) + 1, collapse = ", ")))
         }
    if(any(is.na(herbdat000$STATE_PROVINCE))){
        stop(paste("\"STATE_PROVINCE\" must be provided for row: ", 
             paste(which(is.na(herbdat000$STATE_PROVINCE)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COUNTY))){
        warning(paste("\"COUNTY\" has not been provided for row: ", 
             paste(which(is.na(herbdat000$COUNTY)) + 1, collapse = ", ")))
             herbdat000$COUNTY[is.na(herbdat000$COUNTY)] <- " "
        }
    if(any(is.na(herbdat000$LOCALITY))){
        warning(paste("\"LOCALITY\" not provided  for row: ", 
             paste(which(is.na(herbdat000$LOCALITY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$IDENTIFIED_BY))){
        stop(paste("\"IDENTIFIED_BY\" must be provided for row: ", 
             paste(which(is.na(herbdat000$DETERMINOR)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$DATE_IDENTIFIED))){
        warning(paste("\"DATE_IDENTIFIED\" not provided for row: ", 
             paste(which(is.na(herbdat000$DATE_IDENTIFIED)) + 1, collapse = ", ")))
        }
    formatdate <- function(x){format(as.Date(x),"%d %B %Y")}
    #################### 
    
    dirpgenus <- system.file("extdata", "APGIII_GENERA.csv", 
                              package = "herblabel")
    pgenus <- read.csv(dirpgenus, header = TRUE)
    
    #### Convert the first Letter to capital
    Cap <- function(x) {
        paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
    }
    
    REPLACE <- function(x){
        if(length(x) > 1){
           stop("only one string is allowed")
        }
        bbb <- gsub(",+", ",",gsub(" [[:space:]]+", ",", x))
        endchar <- substr(bbb, nchar(bbb), nchar(bbb))
        if(endchar == ","){ 
            yyy <- gregexpr(pattern = ",", bbb)
            res <- substr(bbb, start = 1, stop = ifelse(unlist(lapply(yyy, function(x){max(x)-1})) > 1, unlist(lapply(yyy, function(x){max(x)-1})) , nchar(bbb)))
        } else {
            res <- bbb
        }
        return(res)
    }


    herbdat000$FAMILY <- toupper(herbdat000$FAMILY)
    herbdat000$FAMILY <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$FAMILY)
    herbdat000$GENUS <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$GENUS)
    herbdat000$SPECIES <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$SPECIES)
    herbdat000$AUTHOR_OF_SPECIES <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$AUTHOR_OF_SPECIES)
    herbdat000$INFRASPECIFIC_RANK <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$INFRASPECIFIC_RANK)
    herbdat000$INFRASPECIFIC_EPITHET <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$INFRASPECIFIC_EPITHET)
    herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK <- gsub("^[[:space:]]+|[[:space:]]+$", "", herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK)
    
    temp1 <- c("{\\rtf1\\ansi\\deff0", #### Staring a RTF 
               "{\\fonttbl{\\f01\\froman\\fcharset01 Times New Roman;}}",    
               "{\\colortbl;\\red0\\green0\\blue0;\\red0\\green0\\blue255;\\red0\\green255\\blue255;
               \\red0\\green255\\blue0;\\red255\\green0\\blue255;\\red255\\green0\\blue0;
               \\red255\\green255\\blue0;\\red255\\green255\\blue255;\\red0\\green0\\blue128;
               \\red0\\green128\\blue128;\\red0\\green128\\blue0;\\red128\\green0\\blue128;
               \\red128\\green0\\blue0;\\red128\\green128\\blue0;\\red128\\green128\\blue128;
               \\red192\\green192\\blue192;}",
                "\\viewkind4\\uc1\\pard\\f01\\fs18\\fi-144\\li288\\ri3480 ",
                "\\paperw11906\\paperh16838\\margt720\\margb360\\margl600\\margr600\\cols2\\colsx720 "
               ) 
    ### fcharset134 to specify Chinese Font
    #### Herbarium Label
    #### Default Font Size if 18
    #### Default font is Time New Roman
    temp2 <- c()
    for(i in 1:nrow(herbdat000)){
        herbdat <- herbdat000[i,]

        ########## Highlighting the names with problem 
        ### Check the genus spelling 
        if(spellcheck){ ### = TRUE
            temp.genus <- herbdat$GENUS
            if(!Cap(as.character(temp.genus)) %in% Cap(as.character(pgenus$GENUS))){
                herbdat$GENUS <- paste("\\highlight6 ", as.character(temp.genus), 
                "\\highlight6 \\i0  (Genus not accepted at The Plant List Website.)\\highlight0 ", sep = "")
            }
            
            #### Check the family spelling 
            temp.family <- herbdat$FAMILY
            if(!Cap(as.character(temp.family)) %in% Cap(as.character(pgenus$FAMILY))){
                herbdat$FAMILY <- paste("\\highlight6 ", as.character(temp.family), 
                "\\highlight6  (Family not accepted at The Plant List Website.) \\highlight0 ", sep = "")
            }
                
            ### check if the family provided in the excel matches APGIII or not, the results 
            ### will be highlighted in yellow 
            fam.genus.temp <- data.frame(FAMILY = as.character(herbdat$FAMILY), 
                                         GENUS = as.character(herbdat$GENUS))
            fgmerge.temp <- merge(x = fam.genus.temp, y = pgenus, by.x = "GENUS", by.y = "GENUS", 
                             all.x = TRUE, sort = FALSE)
            if(any(as.character(Cap(fgmerge.temp$FAMILY.x)) !=  as.character(Cap(fgmerge.temp$FAMILY.y)) 
                        & !is.na(as.character(fgmerge.temp$FAMILY.y)))){
                if(unique(as.character(Cap(fgmerge.temp$FAMILY.x))) 
                          %in% as.character(Cap(fgmerge.temp$FAMILY.y))){
                    herbdat$FAMILY <- paste("\\highlight6 ", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                        " \\highlight0 ", sep = "")
                    herbdat$GENUS <- paste("\\highlight6 ", unique(as.character(fgmerge.temp$GENUS)), 
                                       "\\i0\\highlight6  (could also be under \"", 
                                       paste(as.character(Cap(fgmerge.temp$FAMILY.y))[
                                             !as.character(Cap(fgmerge.temp$FAMILY.y)) %in% 
                                              as.character(Cap(fgmerge.temp$FAMILY.x))], 
                                       collapse = "\", \"") ,
                                       "\" according to The Plant List Website.)\\highlight0 ", sep = "")
                        
                } else{
                herbdat$FAMILY <- paste("\\highlight6 ", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                        "\\highlight0 ", sep = "")
                herbdat$GENUS <- paste("\\highlight6 ", unique(as.character(fgmerge.temp$GENUS)), 
                                       "\\i0 \\highlight6  (should be under \"", 
                                       paste(as.character(fgmerge.temp$FAMILY.y), 
                                       collapse = "\", \"") ,
                                       "\" according to The Plant List Website.)\\highlight0 ", sep = "")
                 }
            }
        }
        res <- c(
        #### Title of the Herbarium
        paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb350\\sa80\\fs20", 
                herbdat$HERBARIUM,"\\par }", sep = ""),
        ####  
        ### FLORA OF SOME PLACE
        ifelse(is.na(herbdat$TITLE)|herbdat$TITLE == "", "", 
               paste("{\\pard\\keep\\keepn\\fi0\\li0\\fs18\\qc\\sb10\\sa100 \\b ",
                herbdat$TITLE,"\\b0 \\par }", sep = "")),
        
        #### FAMILY, in BOLD FACE, must be either in Flora of Hong Kong, or The Plant List
        paste("{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100 \\b ",
               herbdat$FAMILY,"\\b0\\qc0 \\par }", sep = ""),

        #### SPECIES INFO
        ifelse(is.na(herbdat$INFRASPECIFIC_RANK),
              paste("{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs20\\b\\i ",
                    herbdat$GENUS,"\\i0  \\i ", 
                    ifelse((is.na(herbdat$SPECIES)|herbdat$SPECIES == "sp."), 
                    "\\i0 sp.", as.character(herbdat$SPECIES)),"\\i0  ",
                    ifelse(is.na(herbdat$AUTHOR_OF_SPECIES), "", 
                    as.character(herbdat$AUTHOR_OF_SPECIES)), 
                    "\\b0\\par }", sep = ""),
              paste("{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs20\\b\\i ",
                    herbdat$GENUS,"\\i0  \\i ",
                    ifelse((is.na(herbdat$SPECIES)|herbdat$SPECIES == "sp."), 
                    "\\i0 sp.", as.character(herbdat$SPECIES)),"\\i0  ",
                    ifelse(is.na(herbdat$AUTHOR_OF_SPECIES), "", 
                    as.character(herbdat$AUTHOR_OF_SPECIES))," ", 
                    herbdat$INFRASPECIFIC_RANK," \\i ",herbdat$INFRASPECIFIC_EPITHET, "\\i0  ", 
                    herbdat$AUTHOR_OF_INFRASPECIFIC_RANK,"\\b0\\par }", sep = "")),
              
        ##### COUNTY and LOCALITY
        REPLACE(paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb120\\sa20 ", 
        toupper(herbdat$COUNTRY),", ", herbdat$STATE_PROVINCE,
                 ", ", herbdat$COUNTY, ", ", ifelse(is.na(herbdat$LOCALITY), 
                 "", as.character(herbdat$LOCALITY)), "\\par}",sep = "")), 
        
        ##### LONGITUDE, LATITUDE and ELEVATION
        REPLACE(ifelse(is.na(herbdat$LAT_DEGREE), "", 
               paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb20\\sa150\\qj ",
               herbdat$LAT_DEGREE,"\\u176;", herbdat$LAT_MINUTE, "\\u39;",herbdat$LAT_SECOND, 
               "\\u34;", herbdat$LAT_FLAG,", ",herbdat$LON_DEGREE,"\\u176;",herbdat$LON_MINUTE,
               "\\u39;",herbdat$LON_SECOND,"\\u34;", herbdat$LON_FLAG,"; ", 
               herbdat$ELEVATION,"m\\par }",sep = ""))),

        ##### Attributes and Remarks
        REPLACE(ifelse(is.na(herbdat$ATTRIBUTES) & is.na(herbdat$REMARKS)|herbdat$ATTRIBUTES == "" & herbdat$REMARKS == "", "",    
                paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb60", 
                    ifelse(is.na(herbdat$ATTRIBUTES)|herbdat$ATTRIBUTES == "", "", as.character(herbdat$ATTRIBUTES)),
                    ifelse(is.na(herbdat$ATTRIBUTES)|herbdat$ATTRIBUTES == "", "", " "), 
                    ifelse(is.na(herbdat$REMARKS)|herbdat$REMARKS == "", "", as.character(herbdat$REMARKS)), 
                     "\\sa80\\par}", sep = ""))), 
                
        ##### COLLECTOR and COLLECTION NUMBER !
        ifelse(is.na(herbdat$ADDITIONAL_COLLECTOR), 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb50\\sa100\\tqr\\tx4850\\qj\\b ",
                   herbdat$COLLECTOR,", #" ,herbdat$COLLECTOR_NUMBER,"\\b0", 
                   "\\qj0","\\tab ",
                   tryCatch(formatdate(herbdat$DATE_COLLECTED), 
                   error= function(e) {print("Warning: Date format incorrect, using original string"); 
                   herbdat$DATE_COLLECTED}),
                   "\\par}",sep = ""), 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb50\\sa100\\tqr\\tx4850\\qj\\b ",
                   herbdat$COLLECTOR,", ",herbdat$ADDITIONAL_COLLECTOR,"\\qj0  #" ,
                   herbdat$COLLECTOR_NUMBER, "\\b0", "\\tab ",
                   tryCatch(formatdate(herbdat$DATE_COLLECTED), 
                   error= function(e) {print("Warning: Date format incorrect, using original string");
                   herbdat$DATE_COLLECTED}),
                   "\\par}",sep = "")
            ), 
        
        ##### Project
        ifelse(is.na(herbdat$PROJECT)| herbdat$PROJECT== "", "", 
               paste("{\\pard\\keep\\keepn\\fi0\\li0\\sa160\\ql\\b ",
                      as.character(herbdat$PROJECT) ,
                     "\\ql0\\b0\\par }",sep = "")
              ),
        ##### IDENTIFICATION INFOMATION
        ##### "    ", gsub("_", "", as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)), 
        ifelse(!is.na(herbdat$TYPE_STATUS), 
            paste("{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\tqr ",
                  gsub("_", "", ifelse(is.na(as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)), 
                  "", as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER))), 
                  "\\tx4850\\tab ", herbdat$TYPE_STATUS,
                 " \\tql Det.: ",herbdat$IDENTIFIED_BY,", ", 
                 tryCatch(formatdate(herbdat$DATE_IDENTIFIED), 
                 error= function(e) {print("Warning: Date format incorrect, using original string"); 
                 herbdat$DATE_IDENTIFIED}), "\\par}",sep = ""),
            paste("{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\tqr ", gsub("_", 
                  "", ifelse(is.na(as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)), 
                 "", as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER))),
                 "\\tx4850\\tab \\tql Det.: ", herbdat$IDENTIFIED_BY,", ", 
                 tryCatch(formatdate(herbdat$DATE_IDENTIFIED), 
                 error= function(e) {print("Warning: Date format incorrect, using original string"); 
                 herbdat$DATE_IDENTIFIED}), "\\par}",sep = "")
            ),
        "{\\pard\\keep\\keepn\\sa100 \\par }", 
        "{\\pard\\keep\\qc  .    .    .    .    .    .    .    .    .    .    .    . \\par}" 
         )                            ### End of one label
        temp2 <- c(temp2, res)        ### Add label to the RTF file.
    }
    template <- c(temp1, temp2, "}")  ## End of the RTF file
    res <- template[!template %in% ""]
    res <- res[!res %in% " "]
    ###  ### replace multiple commas or space from the string
    writeLines(res, outfile)
    ### Notice
    cat("Herbarium Labels have been saved to:\n", 
         file.path(getwd(), outfile),"\n", sep = "")
}

