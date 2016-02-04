\name{herblabel-package}
\alias{herblabel-package}
\alias{herblabel}
\docType{package}
\title{
Making Labels for Herbarium Specimens in RTF and more.
}
\description{
This package is designed for making herbarium labels and annotation slips based on the information stored in template files. The RTF file can be edited in MS Word or Open Office etc. It also provides a number of conversion utilities.
}
\details{
\tabular{ll}{
Package: \tab herblabel\cr
Type: \tab Package\cr
Version: \tab 0.3.28\cr
Date: \tab 2016-2-4\cr
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
