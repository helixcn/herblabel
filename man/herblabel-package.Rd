\name{herblabel-package}
\alias{herblabel-package}
\alias{herblabel}
\docType{package}
\title{
Making Labels for Herbarium Specimens in RTF and more.
}
\description{
This package provides functionalities for preparing herbarium labels. 
It uses information stored in Darwin Core Herbarium Records Template 
csv files. The outputs are RTF file, so the labels can be 
viewed in MS Word or Open Office, WPS office etc. 
It also provides a number of conversion utilities.
}
\details{
\tabular{ll}{
Package: \tab herblabel\cr
Type: \tab Package\cr
Version: \tab 0.3.23\cr
Date: \tab 2015-11-10\cr
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
    herbarium_label(infile = path,  
                 outfile = "HERBARIUM_LABELS.rtf")
    dat_test <- read.csv(path)
    herbarium_label(dat = dat_test, outfile = "HERBARIUM_LABELS_dat.rtf")
}
