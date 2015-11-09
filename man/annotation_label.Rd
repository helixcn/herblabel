\name{annotation_label}
\alias{annotation_label}
\title{
    Making RTF Annotation labels based on annotation template in CSV
}
\description{
    Making RTF Annotation labels based on annotation template in CSV
}
\usage{
    annotation_label(dat = NULL, infile = NULL, spellcheck = TRUE, outfile = "Annotation_Labels.rtf")
}

\arguments{
  \item{dat}{
data frame of Annotation Template.  
}
  \item{infile}{
Character string, the name of the csv file.
}
  \item{spellcheck}{
Logical, whether the spell checking for species should be porformed. 
}
  \item{outfile}{
the names of the output rtf file.
}
}
\details{
This function reads csv template, and generates an RTF file containing 
annotation labels. 
}

\author{
Jinlong Zhang 
}

\examples{

   path <- system.file("extdata", "ANNOTATION_TEMPLATE.csv", 
                       package = "herblabel")
                       
   annotation_label(infile = path)
   
}
