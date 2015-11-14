#### Create RTF annotation labels 

annotation_label <- function(dat = NULL, infile = NULL, spellcheck = TRUE, outfile = "Annotation_Labels.rtf") {
    if(is.null(dat)&is.null(infile)){
        stop("at least dat or infile should be specified")
    }
    if(!is.null(dat)&!is.null(infile)){
        stop("dat and infile should be not be specified at the same time")
    }
    if(is.null(dat)){
        herbdat000 <- read.csv(infile, header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
    } else {
        herbdat000 <- dat
    }
    
    herbdat000[herbdat000 == ""] <- NA
    
    if (any(is.na(herbdat000$GENUS)|herbdat000$GENUS == "")) {
        warning(paste("\"GENUS\" not provided for row: ", 
            paste(which(is.na(herbdat000$GENUS)|herbdat000$GENUS == "") + 
                1, collapse = ", ")))
    }
    if (any(is.na(herbdat000$IDENTIFIED_BY)|herbdat000$IDENTIFIED_BY == "")) {
        warning(paste("\"IDENTIFIED_BY\" not provided for row: ", 
            paste(which(is.na(herbdat000$IDENTIFIED_BY)|herbdat000$IDENTIFIED_BY == "") + 
                1, collapse = ", ")))
    }
    if (any(is.na(herbdat000$DATE_IDENTIFIED)|herbdat000$DATE_IDENTIFIED == "")) {
        warning(paste("\"DATE_IDENTIFIED\" not provided for row: ", 
            paste(which(is.na(herbdat000$DATE_IDENTIFIED)|herbdat000$IDENTIFIED_BY == "") + 
                1, collapse = ", ")))
    }
    
    formatdate <- function(x){
        format(as.Date(x),"%d %B %Y")
    }
    
    #################### 
    dirpgenus <- system.file("extdata", "APGIII_GENERA.csv", package = "herblabel")
    pgenus    <- read.csv(dirpgenus, header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
    
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
        sptemp <- paste( ifelse(is.na(herbdat000$GENUS)|herbdat000$GENUS=="",                                                 "",  herbdat000$GENUS                       ),
                         ifelse(is.na(herbdat000$SPECIES)|herbdat000$SPECIES=="",                                             "",  herbdat000$SPECIES                     ),
                         ifelse(is.na(herbdat000$AUTHOR_OF_SPECIES)|herbdat000$AUTHOR_OF_SPECIES=="",                         "",  herbdat000$AUTHOR_OF_SPECIES           ),
                         ifelse(is.na(herbdat000$INFRASPECIFIC_RANK)|herbdat000$INFRASPECIFIC_RANK == "",                     "",  herbdat000$INFRASPECIFIC_RANK          ),
                         ifelse(is.na(herbdat000$INFRASPECIFIC_EPITHET)|herbdat000$INFRASPECIFIC_EPITHET == "",               "",  herbdat000$INFRASPECIFIC_EPITHET       ),
                         ifelse(is.na(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK)|herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK == "", "",  herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK), 
                         sep = " ")
        sptemp2 <- c()
        for(i in 1:length(sptemp)){
            sptemp2[i] <- REPLACE(sptemp[i])   
        }
        tplsplistdir <- system.file("extdata", "tplsplist.txt", 
                                   package = "herblabel")
        tplsplist <- readLines(tplsplistdir)
        ind <- (!sptemp2 %in% tplsplist) & (!gsub(" ", "", sptemp2 ) == "")  ### Make sure the empty entries were excluded. 
        herbdat000$GENUS[ind] <- paste("\\cf2\\i0 Name not found. Check Spelling at {\\field{\\*\\fldinst{HYPERLINK \"http://www.theplantlist.org/\"}}{\\fldrslt{\\ul\\cf2 http://www.theplantlist.org/}}} or {\\field{\\*\\fldinst{HYPERLINK \"http://frps.eflora.cn/\"}}{\\fldrslt{\\ul\\cf2 http://frps.eflora.cn/}}} for:\\i  ", herbdat000$GENUS[ind], sep = "")
        herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind] <- paste(ifelse(is.na(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind])|herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind] == "", "", herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind]), "\\cf1", sep = "")
        
   }
    
    ### match.gf(herbdat000$FAMIL, herbdat000$GENUS)
    temp1 <- c("{\\rtf1\\ansi\\deff0", 
                "{\\fonttbl{\\f01\\froman\\fcharset01 Times New Roman;}}", 
                "{\\colortbl;\\red0\\green0\\blue0;\\red0\\green0\\blue255;
                \\red0\\green255\\blue255;\n \\red0\\green255\\blue0;
                \\red255\\green0\\blue255;\\red255\\green0\\blue0;\n
                \\red255\\green255\\blue0;\\red255\\green255\\blue255;
                \\red0\\green0\\blue128;\n \\red0\\green128\\blue128;
                \\red0\\green128\\blue0;\\red128\\green0\\blue128;\n
                \\red128\\green0\\blue0;\\red128\\green128\\blue0;
                \\red128\\green128\\blue128;\n\\red192\\green192\\blue192;}", 
                "\\viewkind4\\uc1\\pard\\f01\\fs19\\fi-144\\li288\\ri3480 ",
                "\\paperw11906\\paperh16838\\margt540\\margb90\\margl600\\margr600\\cols2\\colsx1080\\linebetcol ")
    ### fcharset134 to specify Chinese Font Herbarium Label Default Font Size if 18
    ### Default font is Time New Roman
    temp2 <- c()
    for (i in 1:nrow(herbdat000)) {
        herbdat <- herbdat000[i, ]
        ### Set the size for each label
        res <- c(
        ifelse(is.na(herbdat$TYPE_STATUS)|is.null(herbdat$TYPE_STATUS)|herbdat$TYPE_STATUS == "", 
            "", 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\sb100\\sa50\\fs20\\b ", toupper(as.character(herbdat$TYPE_STATUS)), "\\b0  of:\\par }", sep = "")), 
        ifelse((is.na(herbdat$FAMILY)|is.null(herbdat$FAMILY)|herbdat$FAMILY == ""), 
            "", 
            paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\", ifelse((is.na(herbdat$TYPE_STATUS)|is.null(herbdat$TYPE_STATUS)|herbdat$TYPE_STATUS == ""), "sb20", "sb180"), "\\sa50\\fs20\\b ", toupper(as.character(herbdat$FAMILY)), "\\b0\\par }", sep = "")),
        ifelse(((is.na(herbdat$GENUS)|herbdat$GENUS==""                                                )& 
                (is.na(herbdat$SPECIES)|herbdat$SPECIES==""                                            )& 
                (is.na(herbdat$AUTHOR_OF_SPECIES)|herbdat$AUTHOR_OF_SPECIES==""                        )& 
                (is.na(herbdat$INFRASPECIFIC_RANK)|herbdat$INFRASPECIFIC_RANK == ""                    )& 
                (is.na(herbdat$INFRASPECIFIC_EPITHET)|herbdat$INFRASPECIFIC_EPITHET == ""              )& 
                (is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK)|herbdat$AUTHOR_OF_INFRASPECIFIC_RANK == "")), 
            "", 
            paste("{\\pard\\keep\\keepn\\fi-288\\li288", 
                ifelse((is.na(herbdat$TYPE_STATUS)|herbdat$TYPE_STATUS == "")|(is.na(herbdat$TYPE_STATUS)|is.null(herbdat$TYPE_STATUS)|herbdat$TYPE_STATUS == ""), 
                        "\\sb180", "\\sb20"),
                "\\sa20\\fs20\\b\\i ", 
                ifelse(is.na(herbdat$GENUS)|herbdat$GENUS == "",                          "",      as.character(herbdat$GENUS)), 
                      "\\i0  \\i ", 
                ifelse((is.na(herbdat$SPECIES)|herbdat$SPECIES == ""),                    "\\i0 ", as.character(herbdat$SPECIES)), 
                      "\\i0  ", 
                ifelse(is.na(herbdat$AUTHOR_OF_SPECIES)|herbdat$AUTHOR_OF_SPECIES == "",  "",      as.character(herbdat$AUTHOR_OF_SPECIES)), 
                  " ", 
                ifelse(is.na(herbdat$INFRASPECIFIC_RANK),                                 "",      as.character(herbdat$INFRASPECIFIC_RANK)), 
                " \\i ", 
                ifelse(is.na(herbdat$INFRASPECIFIC_EPITHET),                              "",      as.character(herbdat$INFRASPECIFIC_EPITHET)), 
                "\\i0  ", 
                ifelse(is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK),                       "",      as.character(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK)
                ), 
                "\\b0\\par }", sep = "")
            ), 
        ifelse(is.na(herbdat$TYPE_REF)|herbdat$TYPE_REF == "", 
            "",        
            paste("{\\pard\\keep\\keepn\\fi288\\li288\\brsp20\\sb10\\sa20\\fs16  " , ifelse(is.na(herbdat$TYPE_REF )|herbdat$TYPE_REF == "", "", herbdat$TYPE_REF), " \\par}")
            ), 
        ifelse(is.na(herbdat$DET_NOTE)|herbdat$DET_NOTE == "", 
              "", 
              paste("{\\pard\\keep\\sb10\\sa20\\keepn\\fi0\\li0\\fs16 ", as.character(herbdat$DET_NOTE), " \\par}", sep = "")
            ), 
        paste("{\\pard\\keep", ifelse(((is.na(herbdat$TYPE_STATUS                 )|herbdat$TYPE_STATUS                  == "")&
                                       (is.na(herbdat$TYPE_REF                    )|herbdat$TYPE_REF                     == "")&
                                       (is.na(herbdat$FAMILY                      )|herbdat$FAMILY                       == "")&
                                       (is.na(herbdat$GENUS                       )|herbdat$GENUS                        == "")&
                                       (is.na(herbdat$SPECIES                     )|herbdat$SPECIES                      == "")&
                                       (is.na(herbdat$AUTHOR_OF_SPECIES           )|herbdat$AUTHOR_OF_SPECIES            == "")&
                                       (is.na(herbdat$INFRASPECIFIC_RANK          )|herbdat$INFRASPECIFIC_RANK           == "")&
                                       (is.na(herbdat$INFRASPECIFIC_EPITHET       )|herbdat$INFRASPECIFIC_EPITHET        == "")&
                                       (is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK)|herbdat$AUTHOR_OF_INFRASPECIFIC_RANK == "")&
                                       (is.na(herbdat$DET_NOTE                    )|herbdat$DET_NOTE                     == "")),
                                       "\\sb800",
                                       "\\sb200"), 
             ifelse((is.na(herbdat$PROJECT)|herbdat$PROJECT == ""), 
                   "\\sa150",
                   "\\sa10"), 
             "\\keepn\\fi0\\li0\\tqr\\tx4850 Det.: ", 
                  ifelse(is.na(herbdat$IDENTIFIED_BY)|herbdat$IDENTIFIED_BY == "", 
                         "", 
                         as.character(herbdat$IDENTIFIED_BY)), 
                  "", 
                  ifelse(is.na(herbdat$INSTITUTION)|herbdat$INSTITUTION == "", 
                         "", 
                         paste(", ", as.character(herbdat$INSTITUTION), sep = "")), 
                  "  \\tab ", 
                  ifelse(is.na(herbdat$DATE_IDENTIFIED)|herbdat$DATE_IDENTIFIED == "", 
                         "", 
                         tryCatch(formatdate(herbdat$DATE_IDENTIFIED), 
                         error= function(e) {print("Warning: Date format incorrect, using original string"); 
                                             herbdat$DATE_IDENTIFIED})
                         ), 
                         " \\par }", sep = ""
        ),
        ifelse(is.na(herbdat$PROJECT)|is.null(herbdat$PROJECT)|herbdat$PROJECT == "", 
               "", 
              paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb10\\sa150\\fs16 ", 
                as.character(herbdat$PROJECT), "\\par }", sep = "")
              ),
        "{\\pard\\keep\\qc .                     .                    .\\sa100\\par}" 
             )
        ### End of one label
        temp2 <- c(temp2, res)  ### Add label to the RTF file.
    }
    template <- c(temp1, temp2, "}")  ## End of the RTF file
    res <- template[!template %in% ""]
    writeLines(res, outfile)
    ### Notice
    cat("Annotation labels have been saved to:\n", 
        file.path(getwd(), outfile), "\n", sep = "")
} 
