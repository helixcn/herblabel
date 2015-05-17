\name{wetlabel_rtf}
\alias{wetlabel_rtf}
\title{
Making labels for wet specimens
}
\description{
Making labels for wet specimens
}
\usage{
wetlabel_rtf(infile = NULL, outfile = "Wet_Labels.RTF", spellcheck = FALSE)
}

\arguments{
  \item{infile}{
Character representing the template file in Excel2003.
}
  \item{outfile}{
The name of the RTF file to be generated.
}
  \item{spellcheck}{ 
Logical, indicating whether the spell check will be performed.
}
}
\details{
This function read template in Excel 2003 only, and generate a RTF file
containing herbarium labels. Only a limited number of the columns within
the Excel can be left blank. This function was inspired by the PLable 
software developed at the University of Florida. 
}

\author{
Jinlong Zhang 
}

\examples{

    #Sys.setlocale("LC_TIME", "English")
    path <- system.file("extdata", "HERBARIUM_RECORDS_TEMPLATE.csv", 
                        package = "herblabel")
    wetlabel_rtf(infile = path)
    
}
