\name{herblabel_rtf}
\alias{herblabel_rtf}
\title{
Making herbarium labels
}
\description{
Making herbarium labels in RTF
}
\usage{
herblabel_rtf(infile = NULL, spellcheck = FALSE, outfile = "herblabel.rtf")
}

\arguments{
  \item{infile}{
Character representing the template file in Excel2003.
}
  \item{spellcheck}{ 
Logical, indicating whether the spell check will be performed.
}
  \item{outfile}{
the output rtf file.
}
}
\details{
This function read csv template, and generate a RTF file containing 
herbarium labels. Only a limited number of the columns can be left blank. 
This function was inspired by the PLable software developed at the University
 of Florida Herbarium. 

See The Plabel program developed by Kent D. Perkins at the University of 
Florida Herbarium.
http://www.flmnh.ufl.edu/natsci/herbarium/pl/
}

\author{
Jinlong Zhang 
}

\examples{
    Sys.setlocale("LC_TIME", "English")  
    path <- system.file("extdata", "HERBARIUM_RECORDS_TEMPLATE.csv", 
                        package = "herblabel")
    herblabel_rtf(infile = path,  
                  outfile = "herblabel.csv.HERBARIUM_RECORDS_TEMPLATE.rtf")
    
    path <- system.file("extdata", "res_converted.csv", 
                        package = "herblabel")
    herblabel_rtf(infile = path,  
                  outfile = "herblabel.csv_reconverted.rtf")
    
}
