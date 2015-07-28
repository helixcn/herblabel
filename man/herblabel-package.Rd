\name{herblabel-package}
\alias{herblabel-package}
\alias{herblabel}
\docType{package}
\title{
Preparing Herbarium Labels based on Darwin Core Herbarium Records Template
}
\description{
This package provides funtionalities for preparing herbarium labels. 
It uses information stored in Darwin Core Herbarium Records Template 
csv files. The outputs are RTF or LaTeX file, so the labels can be 
viewed and modified in MS Word or compiled using pdfLatex. 
It also provides a number of conversion utilities.
}
\details{
\tabular{ll}{
Package: \tab herblabel\cr
Type: \tab Package\cr
Version: \tab 0.3.8\cr
Date: \tab 2015-07-28\cr
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
    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.csv", 
                         package = "herblabel")
    ht2herblabel_tex(infile = path,  
                 outfile = "HERBARIUM_LABELS.tex")
    ht2herblabel_rtf(infile = path,  
                 outfile = "HERBARIUM_LABELS.rtf")
}
