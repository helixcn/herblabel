\name{kfbg_new_db2ht}
\alias{kfbg_new_db2ht}
\title{
Converting KFBG new records to Darwin Core herblabel template in CSV
}
\description{
Converting the KFBG new specimen record database to Darwin Core herblabel template in CSV.
}
\usage{
kfbg_new_db2ht(infile, outfile = "herblabel_temp.csv", 
    HERBARIUM_ARGUMENT = "Kadoorie Farm and Botanic Garden Herbarium", 
    HERBARIUM_CODE_ARGUMENT = "KFBG",
    TITLE_ARGUMENT = NA, 
    PROJECT_ARGUMENT = NA, 
    IDENTIFIED_BY_ARGUMENT = NA, 
    DATE_IDENTIFIED_ARGUMENT = NA, 
    PROCESSED_BY_ARGUMENT = NA, 
    DATE_LASTMODIFIED_ARGUMENT = NA)
}

\arguments{
  \item{infile}{
The name of the csv file to be converted. 
}
  \item{outfile}{
The name of the herblabel template csv file to be generated. 
}
  \item{HERBARIUM_ARGUMENT}{
Name of the herbarium
}
  \item{HERBARIUM_CODE_ARGUMENT}{
Code of the herbarium
}
  \item{TITLE_ARGUMENT}{
Title of the collection
}
  \item{PROJECT_ARGUMENT}{
The project the specimen belongs to.
}
  \item{IDENTIFIED_BY_ARGUMENT}{
The people who identified the specimen. 
}
  \item{DATE_IDENTIFIED_ARGUMENT}{
Date of the identification
}
  \item{PROCESSED_BY_ARGUMENT}{
Name of the people who processed the dataset. 
}
  \item{DATE_LASTMODIFIED_ARGUMENT}{
Date of the update made. 
}
}
\details{

The csv file exported from the KFBG New specimen database has the following column names. 

1. PREVIOUS_HERB_NO: Herbarium Number previously assigned. 

2. STORAGE_LOCATION: Storage location

3. DATE_TO_HERB: Date to herbarium

4. COLLECTOR: Name of the collector

5. COLLECTOR_NUMBER: The collector's Number

6. FAMILY: Family

7. SPECIES: Scientific name of the species

8. CHINESE_NAME: Chinese Name of the species

9. COLLECTION_DATE: Date the specimen collection, eg. 12-05-2012 means 12 MAY 2012

10. NUMBER_OF_SPECIEMEN: specimen number

11. NUMBER_OF_COPIES: number of copies. 

12. LOCALITY: precise location where the specimen collected. 

13. ALTITUDE: altitude in meters. 

14. GPS_N: Latitude obtained from GPS. 

15. GPS_E: Longitude obtained from GPS. 

16. NOTES: description of the plant or remarks about the environment. 

17. VERIFIED_BY: People identified the plant. 

18. VERIFICATION_DATE: date of the identification

19. DRY_SPECIMEN: if dry specimen was collected. 

20. SILICA_GEL_SAMPLE: if silica gel sample was collected. 

21. LIVE: if live specimen was collected and transplanted. 

22. SPIRIT: if spirit sample was collected. 

This csv file could be converted to herblabel template csv file for printing herbarium labels. 
Before label printing, additional editing must be made. 
}

\value{
This function returns a dataframe as a Darwin Core herbarium records template, and saves it to a csv file.  
}
\references{
1. The Darwin Core: \url{http://rs.tdwg.org/dwc/}
2. \url{http://www.cvh.ac.cn/news/3}
}
\author{
Jinlong Zhang
}

\seealso{
\code{\link{kfbg_fb2ht}}, \code{\link{scbg_fb2ht}}, \code{\link{kfbg_existing_db2ht}}
}
\examples{

infile <- system.file("extdata", "KFBG_NEW_DB.csv", 
                             package = "herblabel")
kfbg_new_db2ht(infile)

}

\keyword{ format }
\keyword{ conversion }
\keyword{ template }
