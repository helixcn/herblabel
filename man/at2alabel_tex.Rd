\name{at2alabel_tex}
\alias{at2alabel_tex}
\title{
Making annotation labels in Latex based on Darwin Core herbarium records template in CSV
} 
\description{
Making annotation labels in Latex based on Darwin Core herbarium records template in CSV
}
\usage{
at2alabel_tex(infile = NULL,  outfile = "annotation.tex")
}

\arguments{
  \item{infile}{
Character string specifies the csv template. 
}
  \item{outfile}{
The file name for the latex source code.
}
} 
\details{
This function generates LaTeX code containing herbarium annotation labels based on annotation template in CSV. 
The latex file could be compiled to PDF via pdfLatex in MikTeX, TexLive etc.
}
\value{
Annotation labels
}

\author{
Jinlong Zhang
}

\seealso{
\code{\link{at2alabel_rtf}}
}
\examples{
    path <- system.file("extdata", "ANNOTATION_TEMPLATE.csv", 
                         package = "herblabel")
    at2alabel_tex(infile = path)
}
