#### Create RTF annotation labels 

annotation_label <- function(dat = NULL, spellcheck = TRUE, outfile = "Annotation_Labels.rtf") {
    if(is.null(dat)){
        stop("dat should be specified")
    }
    herbdat000 <- dat
    
    herbdat000[herbdat000 == ""] <- NA
    
    if (any(is.na(herbdat000$GENUS))) {
        warning(paste("\"GENUS\" not provided for row: ", 
            paste(which(is.na(herbdat000$GENUS)) + 
                1, collapse = ", ")))
    }
    if (any(is.na(herbdat000$IDENTIFIED_BY))) {
        warning(paste("\"IDENTIFIED_BY\" not provided for row: ", 
            paste(which(is.na(herbdat000$IDENTIFIED_BY)) + 
                1, collapse = ", ")))
    }
    if (any(is.na(herbdat000$DATE_IDENTIFIED))) {
        warning(paste("\"DATE_IDENTIFIED\" not provided for row: ", 
            paste(which(is.na(herbdat000$DATE_IDENTIFIED)) + 
                1, collapse = ", ")))
    }
    
    #### Formating Date
    formatdate <- function(x){
        if(!is.na(suppressWarnings(as.integer(x))) ){
            if(!grepl("^darwin", R.version$os)){  
                x <- as.Date(as.integer(x), origin="1899-12-30")   ### Default in MacOS
            } else {                              
                x <- as.Date(as.integer(x), origin = "1904-01-01") ### Default in Windows
            }
        }
        res <- format(as.Date(x),"%d %B %Y")
        return(res)
    }
    
    pgenus <- herblabel::pgenus
    #### replace multiple commas and white space, and delete comma if it is the last one.
    REPLACE <- function(x){
        if(length(x) > 1){
           stop("only one string is allowed")
        }
        bbb <- gsub(" +", " ", gsub(",+", ", ", gsub(", +", ",", x)))
        bbb <- gsub("^[[:space:]]+|[[:space:]]+$", "", bbb)
        endchar <- substr(bbb, nchar(bbb), nchar(bbb))
        if(endchar == ","){ 
            yyy <- gregexpr(pattern = ",", bbb)
            res <- substr(bbb, start = 1, stop = ifelse(unlist(lapply(yyy, function(x){max(x)-1})) > 1, 
                          unlist(lapply(yyy, function(x){max(x)-1})) , nchar(bbb)))
        } else {
            res <- bbb
        }
        res <- gsub("^[[:space:]]+|[[:space:]]+$", "", res)
        return(res)
    }
    
    if(spellcheck){
        sptemp <- paste( ifelse(is.na(herbdat000$GENUS),                        "",  herbdat000$GENUS                       ),
                         ifelse(is.na(herbdat000$SPECIES),                      "",  herbdat000$SPECIES                     ),
                         ifelse(is.na(herbdat000$AUTHOR_OF_SPECIES),            "",  herbdat000$AUTHOR_OF_SPECIES           ),
                         ifelse(is.na(herbdat000$INFRASPECIFIC_RANK),           "",  herbdat000$INFRASPECIFIC_RANK          ),
                         ifelse(is.na(herbdat000$INFRASPECIFIC_EPITHET),        "",  herbdat000$INFRASPECIFIC_EPITHET       ),
                         ifelse(is.na(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK), "",  herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK), 
                         sep = " ")
        sptemp2 <- c()
        for(i in 1:length(sptemp)){
            sptemp2[i] <- REPLACE(sptemp[i])   
        }
        tplsplist <- herblabel::tplsplist
        ind <- (!sptemp2 %in% tplsplist) & (!gsub(" ", "", sptemp2 ) == "")  ### Make sure the empty entries were excluded. 
        herbdat000$GENUS[ind] <- paste("\\cf2\\i0 Name not found. Check Spelling at {\\field{\\*\\fldinst{HYPERLINK \"http://www.theplantlist.org/\"}}{\\fldrslt{\\ul\\cf2 http://www.theplantlist.org/}}} or {\\field{\\*\\fldinst{HYPERLINK \"http://frps.eflora.cn/\"}}{\\fldrslt{\\ul\\cf2 http://frps.eflora.cn/}}} for:\\i  ", herbdat000$GENUS[ind], sep = "")
        herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind] <- paste(ifelse(is.na(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind]), "", herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind]), "\\cf1", sep = "")
   }
    ### {\\stylesheet {\\qj \\li0 \\ri0 \\widctlpar \\aspalpha \\aspnum \\adjustright \\lin0 \\rin0 \\itap0 \\fs21 \\kerning2 \\dbch \\af2 \\hich \\af0 \\loch \\f0 \\snext0 \\spriority0 Normal;}
    ### match.gf(herbdat000$FAMIL, herbdat000$GENUS)
    ### {\\*\\cs10 \\snext10 \\sunhideused \\spriority99 Default Paragraph Font;}
    temp1 <- "{\\rtf1\\ansi\\ansicpg936\\deflangfe2052\\fcharset134\\deff1{\\fonttbl{\\f0\\froman\\fcharset134 SimSun;}{\\f1\\froman\\fcharset134 Times New Roman;}}{\\stylesheet{\\*\\cs3 Default Paragraph Font;}}{\\colortbl\\red255\\green0\\blue0;\\red0\\green255\\blue0;\\red0\\green0\\blue255;}\\paperw12240\\paperh15840\\margl1800\\margr1800\\margt1440\\margb1440\\gutter0\\ftnbj\\aenddoc\\jcompress1\\viewkind4\\viewscale100\\asianbrkrule\\allowfieldendsel\\snaptogridincell\\viewkind4\\sectd\\sbkpage\\pgwsxn11906\\pghsxn16838\\marglsxn600\\margrsxn600\\margtsxn720\\margbsxn10\\guttersxn0\\headery720\\footery720\\pgbrdropt0\\sectdefaultcl\\cols2\\colsx1080\\linebetcol1\\endnhere"
    ### fcharset134 to specify Chinese Font Herbarium Label Default Font Size if 18
    ### Default font is Time New Roman
    temp2 <- c()
    for (i in 1:nrow(herbdat000)) {
        herbdat <- herbdat000[i, ]
        ### Set the size for each label
        res <- c(
        ifelse((is.na(herbdat$COLLECTOR))|(is.na(herbdat$COLLECTOR_NUMBER)), 
                "", 
                paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\fs18\\sb100\\sa50 Coll.: ", herbdat$COLLECTOR, "  #",herbdat$COLLECTOR_NUMBER, "\\par }", sep = "")), 
        ifelse(is.na(herbdat$TYPE_STATUS), 
            "", 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\sb100\\sa50\\fs20\\b ", toupper(as.character(herbdat$TYPE_STATUS)), "\\b0  of:\\par }", sep = "")), 
        
        ifelse((is.na(herbdat$FAMILY)), 
            "", 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\", ifelse((is.na(herbdat$TYPE_STATUS)), "sb20", "sb180"), "\\sa50\\fs20\\b ", as.character(herbdat$FAMILY), "\\b0\\par }", sep = "")
            ),
        ifelse(((is.na(herbdat$GENUS)                        )& 
                (is.na(herbdat$SPECIES)                      )& 
                (is.na(herbdat$AUTHOR_OF_SPECIES)            )& 
                (is.na(herbdat$INFRASPECIFIC_RANK)           )& 
                (is.na(herbdat$INFRASPECIFIC_EPITHET)        )& 
                (is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK))), 
            "", 
            paste("{\\pard\\keep\\keepn\\fi-288\\li288", 
                ifelse((is.na(herbdat$TYPE_STATUS)), 
                        "\\sb180", "\\sb20"),
                "\\sa20\\fs20\\b\\i ", 
                ifelse(is.na(herbdat$GENUS),                          "",      as.character(herbdat$GENUS)), 
                      "\\i0  \\i ", 
                ifelse((is.na(herbdat$SPECIES)),                      "\\i0 ", as.character(herbdat$SPECIES)), 
                      "\\i0  ", 
                ifelse(is.na(herbdat$AUTHOR_OF_SPECIES),              "",      as.character(herbdat$AUTHOR_OF_SPECIES)), 
                  " ", 
                ifelse(is.na(herbdat$INFRASPECIFIC_RANK),             "",      as.character(herbdat$INFRASPECIFIC_RANK)), 
                " \\i ", 
                ifelse(is.na(herbdat$INFRASPECIFIC_EPITHET),          "",      as.character(herbdat$INFRASPECIFIC_EPITHET)), 
                "\\i0  ", 
                ifelse(is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK),   "",      as.character(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK)
                ), 
                "\\b0\\par }", sep = "")
            ), 
        ifelse(is.na(herbdat$TYPE_REF), 
            "",        
            paste("{\\pard\\keep\\keepn\\fi288\\li288\\brsp20\\sb10\\sa20\\fs16  " , ifelse(is.na(herbdat$TYPE_REF ), "", herbdat$TYPE_REF), " \\par}")
            ), 
        ifelse(is.na(herbdat$DET_NOTE), 
              "", 
              paste("{\\pard\\keep",
                  ifelse(((is.na(herbdat$TYPE_STATUS                              ))&
                                       (is.na(herbdat$TYPE_REF                    ))&
                                       (is.na(herbdat$FAMILY                      ))&
                                       (is.na(herbdat$GENUS                       ))&
                                       (is.na(herbdat$SPECIES                     ))&
                                       (is.na(herbdat$AUTHOR_OF_SPECIES           ))&
                                       (is.na(herbdat$INFRASPECIFIC_RANK          ))&
                                       (is.na(herbdat$INFRASPECIFIC_EPITHET       ))&
                                       (is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK))), "\\sb150", "\\sb10")
                                      ,"\\sa20\\keepn\\fi0\\li0\\fs16 ", as.character(herbdat$DET_NOTE), " \\par}", sep = "")
            ), 
        paste("{\\pard\\keep", ifelse(((is.na(herbdat$TYPE_STATUS                 ))&
                                       (is.na(herbdat$TYPE_REF                    ))&
                                       (is.na(herbdat$FAMILY                      ))&
                                       (is.na(herbdat$GENUS                       ))&
                                       (is.na(herbdat$SPECIES                     ))&
                                       (is.na(herbdat$AUTHOR_OF_SPECIES           ))&
                                       (is.na(herbdat$INFRASPECIFIC_RANK          ))&
                                       (is.na(herbdat$INFRASPECIFIC_EPITHET       ))&
                                       (is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK))&
                                       (is.na(herbdat$DET_NOTE                    ))),
                                       "\\sb800", # if all are missing, extent the space
                                       "\\sb200"), 
             ifelse((is.na(herbdat$PROJECT)), 
                   "\\sa150",
                   "\\sa10"), 
             "\\keepn\\fi0\\li0\\fs18\\tqr\\tx4850 ", 
                  ifelse(is.na(herbdat$ABBREVIATION), 
                         "", 
                         paste(herbdat$ABBREVIATION,": ", sep = "")), 
                  ifelse(is.na(herbdat$IDENTIFIED_BY), 
                         "", 
                         as.character(herbdat$IDENTIFIED_BY)), 
                  "", 
                  ifelse(is.na(herbdat$INSTITUTION), 
                         "", 
                         ifelse(is.na(herbdat$IDENTIFIED_BY), 
                               paste("                         ", as.character(herbdat$INSTITUTION), sep = ""),
                               paste(", ", as.character(herbdat$INSTITUTION), sep = ""))), 
                  "  \\tab ", 
                  ifelse(is.na(herbdat$DATE_IDENTIFIED), 
                         "", 
                         tryCatch(formatdate(herbdat$DATE_IDENTIFIED), 
                                  error= function(e) {print("Warning: Date format incorrect, using original string"); 
                                             herbdat$DATE_IDENTIFIED})
                         ), 
                         " \\par }", sep = ""
        ),
        ifelse(is.na(herbdat$PROJECT), 
               "", 
              paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb10\\sa150\\fs16 ", 
                as.character(herbdat$PROJECT), "\\par }", sep = "")
              ),
        "{\\pard\\keep\\qc\\fs18 .                     .                    .\\sa100\\par}" 
             )
        ### End of one label
        temp2 <- c(temp2, res)  ### Add label to the RTF file.
    }
    template <- c(temp1, temp2, "}")  ## End of the RTF file
    res <- template[!template %in% ""]
    res <- iconv(x = res, from = "UTF-8", to = "GB18030")
    writeLines(res, outfile)
    ### Notice
    cat("Annotation labels have been saved to:\n", 
        file.path(getwd(), outfile), "\n", sep = "")
} 
