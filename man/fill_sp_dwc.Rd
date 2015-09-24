\name{fill_sp_dwc}
\alias{fill_sp_dwc}
\title{
Fill taxonomic information for Darwin Core Template
}
\description{
Search and parse Scientific Names, automatically fill the following columns for each species if not supplied: FAMILY,GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK

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
The function 
(1)looks for the Scientific Name for each species if the Chinese Name is provided as LOCAL_NAME, and it will go to the SCIENTIFIC_NAME column. 
(2) If the full scientific name is also provided as LOCAL_NAME, it will be goes directly to the SCIENTIFIC_NAME column
(3) parse the scientific name to GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK
(4) Search the FAMILY for each species adopted by The Plant List Website.
(5) If any field among FAMILY,GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK has not been provided, the function will try to fill the corresponding fields. 
(6) If there are multiple matches, the function can will issue an warning.
(7) Only the columns in the dat will be selected.  
}
\value{
the dataframe with FAMILY,GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK filled based on the Local Name provided.

}

\author{
Jinlong Zhang
}

\seealso{
See Also as \code{\link{herbarium_label}}
}
\examples{

dat <- read.csv(system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE2.csv",         package = "herblabel"), header = TRUE, stringsAsFactors = FALSE)
fill_sp_dwc(dat)

}

\keyword{ Darwin Core }
