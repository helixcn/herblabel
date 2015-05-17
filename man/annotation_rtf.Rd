\name{annotation_rtf}
\alias{annotation_rtf}
\title{
Making Annotation labels
}
\description{
Making annotation labels in RTF
}
\usage{
annotation_rtf(infile = NULL, outfile = "annotations.rtf")
}

\arguments{
  \item{infile}{
Character strings for the name of the csv file.
}

  \item{outfile}{
the output rtf file.
}
}
\details{
This function reads csv template, and generate an RTF file containing 
annotation labels. 
}

\author{
Jinlong Zhang 
}

\examples{
   path <- system.file("extdata", "ANNOTATION_TEMPLATE.csv", 
                       package = "herblabel")
   annotation_rtf(infile = path,  outfile = "annotations.rtf")
}
