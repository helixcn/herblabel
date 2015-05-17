\name{herblabel-package}
\alias{herblabel-package}
\alias{herblabel}
\docType{package}
\title{
Preparing Labels for Herbarium Specimens
}
\description{
This package provides features for preparing herbarium labels. It uses information 
stored in an Excel 2003 spread sheet. The outputs are RTF or LaTeX file, so the labels can be viewed and be 
modified in MS Word or compiled using pdfLatex.
}
\details{
\tabular{ll}{
Package: \tab herblabel\cr
Type: \tab Package\cr
Version: \tab 0.3.3\cr
Date: \tab 2015-05-01\cr
License: \tab GLP-2\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jinlong Zhang

Maintainer: Jinlong Zhang <jinlongzhang01@gmail.com>
}

\keyword{ package }
\examples{
    path <- system.file("extdata", "HERBARIUM_RECORDS_TEMPLATE.csv", 
                         package = "herblabel")
    herblabel_tex(infile = path,  
                 outfile = "HERBARIUM_RECORDS_TEMPLATE.csv.tex")
    herblabel_rtf(infile = path,  
                 outfile = "HERBARIUM_RECORDS_TEMPLATE.csv.rtf")
}
