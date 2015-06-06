#### Create short herbarium labels, espetially for herbarium spirit samples in RTF

compact_label_rtf <- function(infile = NULL, outfile = "Wet_Labels.RTF", 
                         spellcheck = TRUE)
{
    herbdat000 <- read.csv(infile, header = TRUE, stringsAsFactors = FALSE)
    
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
        stop(paste("\"COUNTY\" must be provided for row: ", 
             paste(which(is.na(herbdat000$COUNTY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$LOCALITY))){
        stop(paste("\"LOCALITY\" must be provided  for row: ", 
             paste(which(is.na(herbdat000$LOCALITY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$REMARKS))){
        stop(paste("\"REMARKS\" must be provided for row: ", 
             paste(which(is.na(herbdat000$REMARKS)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$IDENTIFIED_BY))){
        stop(paste("\"IDENTIFIED_BY\" must be provided for row: ", 
             paste(which(is.na(herbdat000$DETERMINOR)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$DATE_IDENTIFIED))){
        stop(paste("\"DATE_IDENTIFIED\" must be provided for row: ", 
             paste(which(is.na(herbdat000$DATE_IDENTIFIED)) + 1, collapse = ", ")))
        }
    
    #################### 
    
    dirpgenus <- system.file("extdata", "plantlist_genera20141118.csv", package = "herblabel")
    pgenus <- read.csv(dirpgenus, header = TRUE)
    
    Cap <- function(x) {
        paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
    }
 
    ### match.gf(herbdat000$FAMIL, herbdat000$GENUS)

    temp1 <- c("{\\rtf1\\ansi\\deff0", #### Staring a RTF 
               "{\\fonttbl{\\f01\\froman\\fcharset01 Times New Roman;\\f02\\fmodern\\fmorden Arial;}}",    
               "{\\colortbl;\\red0\\green0\\blue0;\\red0\\green0\\blue255;\\red0\\green255\\blue255;
               \\red0\\green255\\blue0;\\red255\\green0\\blue255;\\red255\\green0\\blue0;
               \\red255\\green255\\blue0;\\red255\\green255\\blue255;\\red0\\green0\\blue128;
               \\red0\\green128\\blue128;\\red0\\green128\\blue0;\\red128\\green0\\blue128;
               \\red128\\green0\\blue0;\\red128\\green128\\blue0;\\red128\\green128\\blue128;
               \\red192\\green192\\blue192;}",
               "\\viewkind4\\uc1\\pard\\f01\\fs16\\fi-144\\li288\\ri3480 ",
               "\\paperw11906\\paperh16838\\margl567\\margr567\\margt567\\margb567 "
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
                "\\highlight9 \\i0  (not found in the plantlist genera database.)\\highlight0 ", sep = "")
            }
    
             #### Check the family spelling 
            temp.family <- herbdat$FAMILY
            if(!Cap(as.character(temp.family)) %in% Cap(as.character(pgenus$FAMILY))){
                herbdat$FAMILY <- paste("\\highlight6 ", as.character(temp.family), 
                "\\highlight9  (not found among the APGIII families.)\\highlight0 ", sep = "")
            }
    
            ### check if the family provided in the excel matches APGIII or not, 
        ### the results will be highlighted in yellow 
            fam.genus.temp <- data.frame(FAMILY = as.character(herbdat$FAMILY), 
                                         GENUS = as.character(herbdat$GENUS))
            fgmerge.temp <- merge(x = fam.genus.temp, y = pgenus, by.x = "GENUS", by.y = "GENUS", 
                             all.x = TRUE, sort = FALSE)
            if(any(as.character(Cap(fgmerge.temp$FAMILY.x)) !=  as.character(Cap(fgmerge.temp$FAMILY.y)) 
                        & !is.na(as.character(fgmerge.temp$FAMILY.y)))){
            if(unique(as.character(Cap(fgmerge.temp$FAMILY.x))) %in% as.character(Cap(fgmerge.temp$FAMILY.y))){
                    herbdat$FAMILY <- paste("\\highlight3 ", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                        "\\highlight0 ", sep = "")
                    herbdat$GENUS <- paste("\\highlight3 ", unique(as.character(fgmerge.temp$GENUS)), 
                                       "\\i0 \\highlight9  (could also be in \"", 
                                       paste(as.character(Cap(fgmerge.temp$FAMILY.y))[
                                               !as.character(Cap(fgmerge.temp$FAMILY.y))%in%as.character(Cap(fgmerge.temp$FAMILY.x))], 
                                       collapse = "\", \"") ,
                                       "\" according to the plantlist genera database.)\\highlight0 ", sep = "")
                                       
                } else{
                herbdat$FAMILY <- paste("\\highlight3 ", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                        "\\highlight0 ", sep = "")
                herbdat$GENUS <- paste("\\highlight3 ", unique(as.character(fgmerge.temp$GENUS)), 
                                       "\\i0 \\highlight9  (should be in \"", 
                                       paste(as.character(fgmerge.temp$FAMILY.y), 
                                       collapse = "\", \"") ,
                                       "\" according to the plantlist genera database.)\\highlight0 ", sep = "")
                }
            }
        }
        herbdat$GLOBAL_UNIQUE_IDENTIFIER <- as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)
        res <- c(
        ### Set the size for each label
        "\\margt360\\margb360\\margl360\\margr360\\cols2\\colsx600",
        
        #### Title of the Herbarium
        paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sa80\\qc ", 
                herbdat$HERBARIUM, "      \\b ", gsub("_", "", 
                ifelse(is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER), "", 
                herbdat$GLOBAL_UNIQUE_IDENTIFIER)), "\\b0\\f10\\par }", sep = ""),
        
        #### #### FLORA OF SOME PLACE
        #### ifelse(is.na(herbdat$TITLE), "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\fs20\\sb60 \\b ",
        ####          herbdat$TITLE,"\\b0 \\par }", sep = "")),
        #### SPECIES INFO
        #### FAMILY, in BOLD FACE, must be either in Flora of Hong Kong, or The Plant List
        paste("{\\pard\\keep\\keepn\\fi0\\li0\\b\\qc\\sa60\\fs14 ",
               toupper(herbdat$FAMILY),"\\b0\\par }", sep = ""), 
               
        ifelse(is.na(herbdat$INFRASPECIFIC_RANK),
              paste("{\\pard\\keep\\keepn\\fi-288\\li288\\b\\fs18\\i ",
                    herbdat$GENUS,"\\i0\\i  ", 
                    ifelse((is.na(herbdat$SPECIES)|herbdat$SPECIES == "sp."), "\\i0 sp.", 
                    as.character(herbdat$SPECIES)),"\\i0  ",
                    ifelse(is.na(herbdat$AUTHOR_OF_SPECIES), "", as.character(herbdat$AUTHOR_OF_SPECIES)), 
                    "\\b0\\par }", sep = ""),
              paste("{\\pard\\keep\\keepn\\fi-288\\li288\\b\\fs18\\i ",
                    herbdat$GENUS,"\\i0\\i  ",ifelse((is.na(herbdat$SPECIES)|herbdat$SPECIES == "sp."), 
                    "\\i0 sp.", as.character(herbdat$SPECIES)),"\\i0  ",
                    ifelse(is.na(herbdat$AUTHOR_OF_SPECIES), "", herbdat$AUTHOR_OF_SPECIES), 
                    " ", 
                    herbdat$INFRASPECIFIC_RANK,"\\i ",herbdat$INFRASPECIFIC_EPITHET, "\\i0 ", 
                    herbdat$AUTHOR_OF_INFRASPECIFIC_RANK,"\\b0\\par }", sep = "")),
                    ##### COLLECTOR and COLLECTION NUMBER !
        ifelse(is.na(herbdat$ADDITIONAL_COLLECTOR), 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb80\\tqr\\tx5045\\qj ",
                   herbdat$COLLECTOR,", #" ,herbdat$COLLECTOR_NUMBER,
                   "                             ", 
                   format(as.Date(herbdat$DATE_COLLECTED), format="%d %b %Y"), 
                   "\\qj0\\par}",sep = ""), 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb80\\tqr\\tx5045\\qj ",
                   herbdat$COLLECTOR,
                   ", ",
                   herbdat$ADDITIONAL_COLLECTOR,
                   ", #",
                   herbdat$COLLECTOR_NUMBER, 
                   "                  ",
                   format(as.Date(herbdat$DATE_COLLECTED), format="%d %b %Y"),
                   "\\qj0\\par}",sep = "")
            ), 
        ##### COUNTY and LOCALITY
        paste("{\\pard\\keep\\keepn\\fi0\\li0 ", toupper(herbdat$COUNTRY),", ",
                herbdat$STATE_PROVINCE, ", ", herbdat$COUNTY, ": ",
                ifelse(is.na(herbdat$LOCALITY), "", as.character(herbdat$LOCALITY)), "\\par}",sep = ""), 
        
        ##### LONGITUDE, LATITUDE and ELEVATION
        ifelse(is.na(herbdat$LAT_DEGREE), "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\qj ",
               herbdat$LAT_DEGREE,"\\u176;", herbdat$LAT_MINUTE, "\\u39;",herbdat$LAT_SECOND, 
               "\\u34;", herbdat$LAT_FLAG,", ",herbdat$LON_DEGREE,"\\u176;",herbdat$LON_MINUTE,
               "\\u39;",herbdat$LON_SECOND,"\\u34;", herbdat$LON_FLAG,", ", 
               herbdat$ELEVATION,"m\\par }",sep = "")),
        
        #### COLLECTION NOTES
        ifelse(is.na(herbdat$ATTRIBUTES), "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb100 ", 
                herbdat$ATTRIBUTES,".",ifelse(is.na(herbdat$REMARKS), "\\par", "")," }",sep = "")),

        #### ##### HABITAT,VEGETATION DESCRIPTION AND ASSOCIATED SPECIES
        ifelse(is.na(herbdat$REMARKS), "", 
                 paste("{\\pard\\keep\\keepn\\fi0\\li0\\sa80 ", herbdat$REMARKS, 
                 ".\\par}",sep = "")),
        #### 
        ##### Project
        ifelse(is.na(herbdat$PROJECT), "", 
               paste("{\\pard\\keep\\keepn\\fi0\\li0\\ql\\b ",herbdat$PROJECT ,
                 "\\b0\\ql0\\par }",sep = "")
              ),
        ##### IDENTIFICATION INFOMATION
        ifelse(!is.na(herbdat$TYPE_STATUS), 
            paste("{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\tqr\\tx5045\\qr ", herbdat$TYPE_STATUS,
                 "  Det.: ",herbdat$IDENTIFIED_BY,", ", format(as.Date(herbdat$DATE_IDENTIFIED), 
                 format="%d %b %Y"), " \\qr0\\par }",sep = ""),
            paste("{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\tqr\\tx5045\\qr Det.: ",
                 herbdat$IDENTIFIED_BY,", ", format(as.Date(herbdat$DATE_IDENTIFIED), 
                 format="%d %b %Y"), " \\qr0\\par }",sep = "")
            ),
        "{\\pard\\sa120 \\par }"
         )                            ### End of one label
        temp2 <- c(temp2, res)        ### Add label to the RTF file.
    }
    template <- c(temp1, temp2, "}") ## End of the RTF file
    res <- template[!template %in% ""]
    writeLines(res, outfile)
    ### Notice
    cat("Compact herbarium labels have been saved to:\n", 
        file.path(getwd(), outfile),"\n", sep = "")
}
