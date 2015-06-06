#### Create RTF annotation labels 

at2alabel_rtf <- function(infile = NULL, outfile = "annotations.rtf") {
    herbdat000 <- read.csv(infile, header = TRUE)
    if (any(is.na(herbdat000$GENUS))) {
        stop(paste("\"GENUS\" must be provided for row: ", 
        paste(which(is.na(herbdat000$GENUS)) + 
            1, collapse = ", ")))
    }
    if (any(is.na(herbdat000$IDENTIFIED_BY))) {
        stop(paste("\"IDENTIFIED_BY\" must be provided for row: ", 
        paste(which(is.na(herbdat000$DETERMINOR)) + 
            1, collapse = ", ")))
    }
    if (any(is.na(herbdat000$DATE_IDENTIFIED))) {
        stop(paste("\"DATE_IDENTIFIED\" must be provided for row: ", 
        paste(which(is.na(herbdat000$DATE_IDENTIFIED)) + 
            1, collapse = ", ")))
    }
    
    #################### 
    dirpgenus <- system.file("extdata", "plantlist_genera20141118.csv", 
                            package = "herblabel")
    pgenus <- read.csv(dirpgenus, header = TRUE)
    
    Cap <- function(x) {
        paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
    }
    
    ### match.gf(herbdat000$FAMIL, herbdat000$GENUS)
    temp1 <- c("{\\rtf1\\ansi\\deff0", 
                "{\\fonttbl{\\f01\\froman\\fcharset01 Times New Roman;
                \\f02\\fmodern\\fmorden Arial;}}", 
                "{\\colortbl;\\red0\\green0\\blue0;\\red0\\green0\\blue255;
                \\red0\\green255\\blue255;\n \\red0\\green255\\blue0;
                \\red255\\green0\\blue255;\\red255\\green0\\blue0;\n
                \\red255\\green255\\blue0;\\red255\\green255\\blue255;
                \\red0\\green0\\blue128;\n \\red0\\green128\\blue128;
                \\red0\\green128\\blue0;\\red128\\green0\\blue128;\n
                \\red128\\green0\\blue0;\\red128\\green128\\blue0;
                \\red128\\green128\\blue128;\n\\red192\\green192\\blue192;}", 
                "\\viewkind4\\uc1\\pard\\f01\\fs18\\fi-144\\li288\\ri3480 ", 
                "\\paperw11906\\paperh16838\\margl567\\margr567\\margt567\\margb567 ")
    ### fcharset134 to specify Chinese Font Herbarium Label Default Font Size if 18
    ### Default font is Time New Roman
    temp2 <- c()
    for (i in 1:nrow(herbdat000)) {
        herbdat <- herbdat000[i, ]
        ### Set the size for each label
        res <- c("\\margt360\\margb360\\margl360\\margr360\\cols2\\colsx600", 
        ifelse(is.na(herbdat$PROJECT), 
            "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb400\\sa100\\fs16", 
                as.character(herbdat$PROJECT), "\\par }", sep = "")), 
                ifelse(is.na(herbdat$INFRASPECIFIC_RANK), 
            paste("{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs20\\b\\i ", 
                herbdat$GENUS, "\\i0  \\i ", ifelse((is.na(herbdat$SPECIES) | herbdat$SPECIES == 
                  "sp."), "\\i0 sp.", as.character(herbdat$SPECIES)), "\\i0  ", 
                  ifelse(is.na(herbdat$AUTHOR_OF_SPECIES), 
                  "", as.character(herbdat$AUTHOR_OF_SPECIES)), "\\b0\\par }", sep = ""), 
            paste("{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs20\\b\\i ", 
                herbdat$GENUS, "\\i0  \\i ", ifelse((is.na(herbdat$SPECIES) | herbdat$SPECIES == 
                  "sp."), "\\i0 sp.", as.character(herbdat$SPECIES)), "\\i0  ", 
                  ifelse(is.na(herbdat$AUTHOR_OF_SPECIES), 
                  "", as.character(herbdat$AUTHOR_OF_SPECIES)), " ", herbdat$INFRASPECIFIC_RANK, 
                " \\i ", herbdat$INFRASPECIFIC_EPITHET, "\\i0  ", herbdat$AUTHOR_OF_INFRASPECIFIC_RANK, 
                "\\b0\\par }", sep = "")), ifelse(is.na(herbdat$DET_SOURCE), "", 
                  paste("{\\pard\\keep\\sb100\\sa50\\keepn\\fi0\\li0Det. Source:", 
                  as.character(herbdat$DET_SOURCE), "}")), 
                  paste("{\\pard\\keep\\sb150\\sa50\\keepn\\fi0\\li0\\tqr\\tx5045 Det.: ", 
            herbdat$IDENTIFIED_BY, ", ", herbdat$INSTITUTION, "  \\tab ", 
                 format(as.Date(herbdat$DATE_IDENTIFIED), 
                format = "%d %b %Y"), " \\par }", sep = ""), "{\\pard\\sa400 \\par }")
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
