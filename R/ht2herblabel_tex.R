#### Create herbarium labels in LaTeX

ht2herblabel_tex <- function(infile = NULL, spellcheck = TRUE, outfile = "herblabel.tex"){
    #Sys.setlocale("LC_TIME", "English")
    herbdat000 <- read.csv(infile, header = TRUE, stringsAsFactors = FALSE)
	
    if(any(is.na(herbdat000$HERBARIUM))){
        stop(paste("\"HERBARIUM\" not provided for row: ", 
             paste(which(is.na(herbdat000$HERBARIUM))+1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COLLECTOR))){
        stop(paste("\"COLLECTOR\" not provided for row: ", 
             paste(which(is.na(herbdat000$COLLECTOR))+1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COLLECTOR_NUMBER))){
        stop(paste("\"COLLECTOR_NUMBER\" not provided for row: ", 
             paste(which(is.na(herbdat000$COLLECTOR_NUMBER)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$DATE_COLLECTED))){
        stop(paste("\"DATE_COLLECTED\" not provided for row: ", 
             paste(which(is.na(herbdat000$DATE_COLLECTED)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$FAMILY))){
        stop(paste("\"FAMILY\" not provided for row: ", 
             paste(which(is.na(herbdat000$FAMILY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$GENUS))){
        stop(paste("\"GENUS\" not provided for row: ", 
             paste(which(is.na(herbdat000$GENUS)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COUNTRY))){
        stop(paste("\"COUNTRY\" not provided for row: ", 
             paste(which(is.na(herbdat000$COUNTRY)) + 1, collapse = ", ")))
         }
    if(any(is.na(herbdat000$STATE_PROVINCE))){
        stop(paste("\"STATE_PROVINCE\" not provided for row: ", 
             paste(which(is.na(herbdat000$STATE_PROVINCE)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$COUNTY))){
        stop(paste("\"COUNTY\" not provided for row: ", 
             paste(which(is.na(herbdat000$COUNTY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$LOCALITY))){
        stop(paste("\"LOCALITY\" not provided  for row: ", 
             paste(which(is.na(herbdat000$LOCALITY)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$REMARKS))){
        warning(paste("\"REMARKS\" not provided for row: ", 
             paste(which(is.na(herbdat000$REMARKS)) + 1, collapse = ", ")))
        }
    if(any(is.na(herbdat000$IDENTIFIED_BY))){
        stop(paste("\"IDENTIFIED_BY\" not provided for row: ", 
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

    temp1 <- c("\\documentclass[a4paper,5pt,twocolumn]{article}",
                "\\pagestyle{empty} ",
                "\\usepackage{geometry}", # 
                "\\usepackage{CJK}",
                "\\geometry{left=0.8cm,right=0.1cm,top=0.8cm,bottom=0.1cm}",
                "\\usepackage[usenames]{color}",
                "\\usepackage{mathptmx}",
                "\\usepackage{times}",
                "\\begin{document}",
                "\\begin{CJK}{GBK}{song}", 
                "\\setlength{\\parindent}{0pt}", 
                "\\fontsize{9pt}{9pt}"
               ) 
    ### fcharset134 to specify Chinese Font
    #### Herbarium Label
    #### Default Font Size if 18
    #### Default font is Time New Roman
    temp2 <- c()
    for(i in 1:nrow(herbdat000)){
        herbdat <- herbdat000[i,]
        if(spellcheck){
            ########## Highlighting the names with problem 
            ### Check the genus spelling 
            temp.genus <- herbdat$GENUS
            if(!Cap(as.character(temp.genus)) %in% Cap(as.character(pgenus$GENUS))){
                herbdat$GENUS <- paste("{\\color{red}{ ", as.character(temp.genus), 
                " (not found in the plantlist genera database.)}}", sep = "")
            }
            
            #### Check the family spelling 
            temp.family <- herbdat$FAMILY
            if(!Cap(as.character(temp.family)) %in% Cap(as.character(pgenus$FAMILY))){
                herbdat$FAMILY <- paste("{\\color{red}{", as.character(temp.family), 
                     " (not found among the APGIII families.)}}", sep = "")
            }
            
            ### check if the family provided in the excel matches APGIII or not, the results will be highlighted in yellow 
            fam.genus.temp <- data.frame(FAMILY = as.character(herbdat$FAMILY), 
                                         GENUS = as.character(herbdat$GENUS))
            fgmerge.temp <- merge(x = fam.genus.temp, y = pgenus, by.x = "GENUS", by.y = "GENUS", 
                             all.x = TRUE, sort = FALSE)
            if(any(as.character(Cap(fgmerge.temp$FAMILY.x)) !=  as.character(Cap(fgmerge.temp$FAMILY.y)) 
                        & !is.na(as.character(fgmerge.temp$FAMILY.y)))){
                if(unique(as.character(Cap(fgmerge.temp$FAMILY.x))) %in% as.character(Cap(fgmerge.temp$FAMILY.y))){
                    herbdat$FAMILY <- paste("{\\color{red}{ ", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                        "}} ", sep = "")
                    herbdat$GENUS <- paste("{\\color{red}{ ", unique(as.character(fgmerge.temp$GENUS)), 
                                       " (could also be in: ", 
                                       paste(as.character(Cap(fgmerge.temp$FAMILY.y))[!as.character(
                                            Cap(fgmerge.temp$FAMILY.y))%in%as.character(Cap(fgmerge.temp$FAMILY.x))], 
                                       collapse = ",", "") ,
                                       " according to the plantlist genera database.)}} ", sep = "")
                } else{
                herbdat$FAMILY <- paste("{\\color{red}{", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                        "} }", sep = "")
                herbdat$GENUS <- paste("{\\color{red}{", unique(as.character(fgmerge.temp$GENUS)), 
                                       "  (should be in: ", 
                                       paste(as.character(fgmerge.temp$FAMILY.y), 
                                       collapse = ", ") ,
                                       " according to the plantlist genera database.)}}", sep = "")
                }
            }
        }
        res <- c(
        ### Set the size for each label
        "\\begin{tabular}{@{}p{8.6cm}}",
        
        #### Title of the Herbarium
        paste("\\centerline{", 
                herbdat$HERBARIUM, "\\vspace{ 1mm }}", sep = ""),
        
        #### FLORA OF SOME PLACE
        #### ifelse(is.na(herbdat$TITLE), "", paste("\\centerline{ ",
        ####         herbdat$TITLE,"}\\vspace{ 2mm }", sep = "")),
        
        #### Must follow the The Plant List Website
        paste("\\centerline{",herbdat$FAMILY,"}\\vspace{ 3mm }", sep = ""),
        "\\large{",
        #### SPECIES INFO
        ifelse(is.na(herbdat$INFRASPECIFIC_RANK),
              paste("\\textbf{\\textsl{",
                    herbdat$GENUS,"}", 
                    ifelse(is.na(herbdat$SPECIES)|herbdat$SPECIES == "sp.", " sp.", 
                    paste(" \\textsl{", as.character(herbdat$SPECIES),"}")), "", 
                    ifelse(is.na(herbdat$AUTHOR_OF_SPECIES),"", 
                    as.character(herbdat$AUTHOR_OF_SPECIES)), 
                    "}", sep = ""),
              paste("\\textbf{\\textsl{",
                    herbdat$GENUS,"} \\textsl{",herbdat$SPECIES,"} ",
                    herbdat$AUTHOR_OF_SPECIES," ", herbdat$INFRASPECIFIC_RANK,
                    " \\textsl{",herbdat$INFRASPECIFIC_EPITHET, "} ", 
                    ifelse(is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK),"", 
                    as.character(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK)) ,"} ", sep = "")
                ),
        "}\\\\", 
        "\\vspace{2mm }",        
        ##### COUNTY and LOCALITY
        paste("", toupper(herbdat$COUNTRY),", ",
                herbdat$STATE_PROVINCE, ", ", herbdat$COUNTY, ": ", herbdat$LOCALITY, ". \\\\",sep = ""), 
        ##### LONGITUDE, LATITUDE and ELEVATION
        ifelse(is.na(herbdat$LAT_DEGREE), "", paste(
               herbdat$LAT_DEGREE,"$\\,^{\\circ}$", 
               herbdat$LAT_MINUTE, "\\textsf{'}",herbdat$LAT_SECOND, 
               "\\textsf{''}", herbdat$LAT_FLAG,", ",
               herbdat$LON_DEGREE,"$\\,^{\\circ}$",herbdat$LON_MINUTE,
               "\\textsf{'}",herbdat$LON_SECOND,"\\textsf{''}", 
               herbdat$LON_FLAG,",  Alt.:", 
               herbdat$ELEVATION,"m \\\\",sep = "")),
        "\\vspace{1.5mm }",
        ##### Attributes and Remarks
        paste(ifelse(is.na(herbdat$ATTRIBUTES),"", as.character(herbdat$ATTRIBUTES)), 
              "  ", ifelse(is.na(herbdat$REMARKS),"", as.character(herbdat$REMARKS))," \\\\ ", sep = ""), 
              "\\vspace{1mm}", 
        
        ##### COLLECTOR and COLLECTION NUMBER !
        ifelse(is.na(herbdat$ADDITIONAL_COLLECTOR), 
            paste("\\textbf{",
                   herbdat$COLLECTOR,"} \\textbf{ $\\#$ " ,
                   herbdat$COLLECTOR_NUMBER,"}\\hfill " ,
                   format(as.Date(herbdat$DATE_COLLECTED), 
                          format="%d %B %Y"),
                   "\\\\",sep = ""), 
            paste("\\textbf{",
                   herbdat$COLLECTOR,"}, \\textbf{",
                   herbdat$ADDITIONAL_COLLECTOR,"} \\textbf{ $\\#$ " ,
                   herbdat$COLLECTOR_NUMBER,"}\\hfill " ,
                   format(as.Date(herbdat$DATE_COLLECTED), 
                   format="%d %B %Y"),"\\\\",sep = "")
            ), 
        ##### Project
        ifelse(is.na(herbdat$PROJECT), "", 
               paste("\\textbf{",as.character(herbdat$PROJECT),"}\\\\",sep = "")), 
        "\\vspace{0.5mm}",
        ##### IDENTIFICATION INFOMATION
        ifelse(!is.na(herbdat$TYPE_STATUS), 
            paste("\\rightline{", gsub("_", "", 
            ifelse(is.na(as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)), "", 
            as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER))) ,
            " \\hfill ", herbdat$TYPE_STATUS,
                 "  Det.: ",herbdat$IDENTIFIED_BY,", ", 
                 format(as.Date(herbdat$DATE_IDENTIFIED), 
                 format="%d %B %Y"), "}\\\\",sep = ""),
            paste("\\rightline{", gsub("_", "", 
                 ifelse(is.na(as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)), "", 
                 as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER)))," \\hfill ", "Det.: ",
                 herbdat$IDENTIFIED_BY,", ", 
                 format(as.Date(herbdat$DATE_IDENTIFIED), 
                 format="%d %B %Y"), "}\\\\",sep = "")
            ),
        "\\vspace{3mm}",
        "\\end{tabular}\\\\"
         )                            ### End of one label
        temp2 <- c(temp2, res)        ### Add label to the RTF file.
    }
    template <- c(temp1, 
                  temp2,
                  "\\end{CJK}", 
                  "\\end{document}") ## End of the RTF file
                ### Replace the illegal character &
    template <- gsub("&", "\\\\&", template)
    res <- template[!template %in% ""]
    writeLines(res, outfile)
    ### Notice
    cat("Herbarium Labels have been saved to:\n", 
        file.path(getwd(), outfile), "\n", sep = "")
}
