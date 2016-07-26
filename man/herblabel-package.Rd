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
\packageDESCRIPTION{herblabel}
\packageIndices{herblabel}
}

\author{
Jinlong Zhang

Maintainer: Jinlong Zhang <jinlongzhang01@gmail.com>
}

\keyword{ package }
\examples{
    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS.xlsx", 
                         package = "herblabel")
    library(openxlsx)
    dat_test <- read.xlsx(path)
    herbarium_label(dat = dat_test, outfile = "HERBARIUM_LABELS_dat.rtf")
}
