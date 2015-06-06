#### Create annotation labels in Latex

at2alabel_tex <- function(infile = NULL, outfile = "annotation.tex"){
    herbdat000 <- read.csv(infile, header = TRUE)
    temp1 <- c("\\documentclass[a4paper,5pt,twocolumn]{article}",
                "\\pagestyle{empty} ",
                "\\usepackage{geometry}",
                "\\usepackage{CJK}",
                "\\geometry{left=0.8cm,right=0.2cm,
                 top=0.8cm,bottom=0.2cm}",
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
        res <- c(
        ### Set the size for each label
        "\\begin{tabular}{@{}p{8.5cm}}",
        #### Title of the Herbarium
        ifelse(is.na(herbdat$PROJECT), "\\vspace{4mm}", 
            paste("\\centerline{", as.character(herbdat$PROJECT), 
            "}\\vspace{1mm}", sep = "")),
        #### SPECIES INFO
        ifelse(is.na(herbdat$INFRASPECIFIC_RANK),
              paste("\\large{\\textsl{\\textbf{",
                    herbdat$GENUS,"}}", ifelse(is.na(herbdat$SPECIES)|
                    herbdat$SPECIES == "sp.", " \\textbf{ sp.}", 
                    paste(" \\textsl{\\textbf{", as.character(
                    herbdat$SPECIES),"}}")), "\\textbf{", 
                    ifelse(is.na(herbdat$AUTHOR_OF_SPECIES),"", 
                    as.character(herbdat$AUTHOR_OF_SPECIES)), 
                    "}}", sep = ""),
              paste("\\large{\\textsl{\\textbf{",
                    herbdat$GENUS,"}} \\textsl{\\textbf{",
                    herbdat$SPECIES,"}} \\textbf{",
                    herbdat$AUTHOR_OF_SPECIES,"} ", 
                    herbdat$INFRASPECIFIC_RANK,
                    " \\textsl{\\textbf{",herbdat$INFRASPECIFIC_EPITHET, 
                    "}} \\textbf{", 
                    ifelse(is.na(herbdat$AUTHOR_OF_INFRASPECIFIC_RANK),
                    "", as.character(
                    herbdat$AUTHOR_OF_INFRASPECIFIC_RANK)) ,"}} ", 
                    sep = "")
                ),
        "\\vspace{4mm }\\\\", 
        ifelse(is.na(herbdat$DET_SOURCE), "", 
            paste("Det. Source: ", herbdat$DET_SOURCE, "\\\\",sep = "")), 
        ##### IDENTIFICATION INFOMATION
        paste("\\rightline{",  "Det.: ", herbdat$IDENTIFIED_BY, " ",
        herbdat$INSTITUTION," \\hfill ", 
                 " ", format(as.Date(herbdat$DATE_IDENTIFIED), 
                 format="%d %b %Y"), "}\\\\",sep = ""), 
        "\\vspace{3mm}\\\\", 
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
    cat("Annotation Labels have been saved to:\n", 
         file.path(getwd(), outfile),"\n", sep = "")
}
