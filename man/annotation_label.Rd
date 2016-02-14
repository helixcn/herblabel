\name{annotation_label}
\alias{annotation_label}
\title{
    Making RTF Annotation labels based on annotation template
}
\description{
    Making RTF Annotation labels based on annotation template
}
\usage{
    annotation_label(dat = NULL, spellcheck = TRUE, outfile = "Annotation_Labels.rtf")
}

\arguments{
  \item{dat}{
data frame of Annotation Template.  
}
  \item{spellcheck}{
Logical, whether the spell checking for species should be porformed. 
}
  \item{outfile}{
the names of the output rtf file.
}
}
\details{
This function reads csv or data frame "annoation template", and generates RTF file containing 
annotation labels. 

\code{PROJECT}: project related with this annotation. eg. Examined for the Revision for the Fern Flora of Hong Kong

\code{TYPE_STATUS}: which kind of type specimen: eg. ISOTYPE

\code{TYPE_REF}: what reference the identification was based upon. DC. Monogr. Phanerog. 6: 409. 1889.

\code{FAMILY}: Family, eg. Magnoliaceae

\code{GENUS}:Genus, eg. Ophioglossum

\code{SPECIES}:Species, eg. petiolatum

\code{AUTHOR_OF_SPECIES}: Author, eg. Hook.

\code{INFRASPECIFIC_RANK}: Infra-specific Rank, eg. var., f. or subsp. 

\code{INFRASPECIFIC_EPITHET}: Epithet for the infra specific rank. 

\code{AUTHOR_OF_INFRASPECIFIC_RANK}: Author for the infra specific rank. 

\code{ABBREVIATION}: Role of the person annotating this herbarium specimen, eg. "Det.", "Verified by".

\code{IDENTIFIED_BY}:Who identified the species, eg. Jin-Long Zhang

\code{INSTITUTION}:The institution where the identification was made, eg. KFBG

\code{DATE_IDENTIFIED}: should follow the format: 2014-12-19 for "19 December 2014". 

\code{DET_NOTE}:The specimen is extremely close to Ranunculus japonicus

\code{SPECIMEN_NUMBER}: Global Unique Identifier, see the help file for \code{\link{herbarium_label}} for more information. 

\code{COLLECTOR}: Collector of this specimen

\code{COLLECTOR_NUMBER}: collector's number of this specimen.

}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{

    path <- system.file("extdata", "ANNOTATION_TEMPLATE.xlsx", 
                       package = "herblabel")
    library(openxlsx)
    dat <- read.xlsx(path)                    
    annotation_label(dat)
   
}
