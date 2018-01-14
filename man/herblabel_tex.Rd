\name{herblabel_tex}
\alias{herblabel_tex}
\title{
Make herbarium labels in Latex
}
\description{
Make herbarium labels in Latex
}
\usage{
herblabel_tex(infile = NULL, spellcheck = FALSE, outfile = "herblabel.tex")
}

\arguments{
  \item{infile}{
Character string specifies the excel template. 
}
  \item{spellcheck}{
Whether the check of spelling and match of the genera and families will be performed
}
  \item{outfile}{
The file name for the latex code.
}
}
\details{
If the spell check is TRUE, both the genera and families will be compared with the accepted genera from TPL (the plantlist). 
}
\value{
This function will create a Latex file containing the herbarium labels. The latex file could be compiled to PDF via pdfLatex in Miktex, TexWorks, TexLive etc. The text will be highlighted if the genus does not match to the family. Comments will be automatically generated. If you do not want to change the information, just change "spellcheck = FALSE". 
}
\references{
The Plabel program developed by Kent D. Perkins at the  University of Florida Herbarium.
 http://www.flmnh.ufl.edu/natsci/herbarium/pl/
}
\author{
Jinlong Zhang
}

\seealso{
\code{\link{herblabel_rtf}}
}
\examples{
path <- system.file("extdata", "herbarium_records.xls", package = "herblabel")
herblabel_tex(infile = path)
}
