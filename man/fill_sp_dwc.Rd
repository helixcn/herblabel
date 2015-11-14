\name{fill_sp_dwc}
\alias{fill_sp_dwc}
\title{
Filling taxonomic information based on Local Name for Darwin Core Template 
}
\description{
Searching and parsing Scientific Names in the Local Name field, and automatically filling the following columns for each species if not supplied: FAMILY,GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK

}
\usage{
fill_sp_dwc(dat)
}

\arguments{
  \item{dat}{
    Data frame, usually a dataframe representing Darwin Core Herbarium Templates
}
}
\details{
This function will:
 
(1) Look for the Scientific Name for each species if the Chinese Name is provided as LOCAL_NAME, and the scientific name will go to the SCIENTIFIC_NAME column. 

(2) If the full scientific name is provided as LOCAL_NAME, it will goes directly to the SCIENTIFIC_NAME column.

(3) Parse the scientific name, and fill the following the columns: GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET and AUTHOR_OF_INFRASPECIFIC_RANK.

(4) Search the FAMILY for each species based on GENUS, that adopted by The Plant List Website. 

(5) If any field: FAMILY, GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK has not been provided, the function will try to fill the corresponding fields. 

(6) If there are multiple matches, the function can will issue an warning. Pay attention to the multiple matches.

(7) Only the orginal columns will be kept in the final result.  

}
\value{

The dataframe containing FAMILY, GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK for a given species.

}

\author{
Jinlong Zhang
}

\seealso{
\code{\link{herbarium_label}}
}
\examples{

syst <- Sys.info()[['sysname']]
if(syst == "Windows"){
    dat <- read.csv(system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE2_win.csv", package = "herblabel"), header = TRUE, stringsAsFactors = FALSE)
} else {
    dat <- read.csv(system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE2.csv", package = "herblabel"), header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
}

dwc_filled <- fill_sp_dwc(dat)
herbarium_label(dat = dwc_filled, outfile = "herblabel_fill_sp_dwc_example.rtf")

}

\keyword{ Darwin Core }
