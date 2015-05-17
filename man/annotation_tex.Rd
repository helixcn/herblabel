\name{annotation_tex}
\alias{annotation_tex}
\title{
Make annotation labels in Latex
}
\description{
Make herbarium labels in Latex
}
\usage{
annotation_tex(infile = NULL,  outfile = "annotation.tex")
}

\arguments{
  \item{infile}{
Character string specifies the excel template. 
}
  \item{outfile}{
The file name for the latex source code.
}
}
\details{
This function generates herbarium annotation labels based on the template of 
annotations. This function will create a Latex file containing the annotation 
labels. The latex file could be compiled to PDF via pdfLatex in MikTeX, 
TexLive etc.
}
\value{
Annotation labels
}

\author{
Jinlong Zhang
}

\seealso{
\code{\link{annotation_rtf}}
}
\examples{
path <- system.file("extdata", "ANNOTATION_TEMPLATE.csv", 
                    package = "herblabel")
annotation_tex(infile = path)
}
