\name{at2alabel_rtf}
\alias{at2alabel_rtf}
\title{
    Making RTF Annotation labels based on annotation template in CSV
}
\description{
    Making RTF Annotation labels based on annotation template in CSV
}
\usage{
    at2alabel_rtf(infile = NULL, outfile = "annotations.rtf")
}

\arguments{
  \item{infile}{
Character string, the name of the csv file.
}

  \item{outfile}{
the names of the output rtf file.
}
}
\details{
This function reads csv template, and generates an RTF file containing 
annotation labels. 
}

\seealso{
\code{\link{at2alabel_tex}}
}

\author{
Jinlong Zhang 
}

\examples{

   path <- system.file("extdata", "ANNOTATION_TEMPLATE.csv", 
                       package = "herblabel")
                       
   at2alabel_rtf(infile = path,  outfile = "annotations.rtf")
   
}
