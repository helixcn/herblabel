\name{kfbg_fb2ht}
\alias{kfbg_fb2ht}
\title{
Converting KFBG field book template format to Darwin Core herblabel template in CSV
}
\description{
Converting the KFBG field book template csv file to Darwin Core herblabel template in CSV 
}
\usage{
kfbg_fb2ht(infile, outfile = "herblabel_temp.csv", 
    HERBARIUM_ARGUMENT = "Kadoorie Farm and Botanic Garden Herbarium", 
    HERBARIUM_CODE_ARGUMENT = "KFBG", 
    TITLE_ARGUMENT = NA, PROJECT_ARGUMENT = NA, 
    IDENTIFIED_BY_ARGUMENT = NA, 
    DATE_IDENTIFIED_ARGUMENT = NA, 
    PROCESSED_BY_ARGUMENT = NA, 
    DATE_LASTMODIFIED_ARGUMENT = NA)
}
\arguments{
  \item{infile}{
Name of The KFBG field book csv template. 
}
  \item{outfile}{
Name of the herblabel csv template to be converted. 
}
  \item{HERBARIUM_ARGUMENT}{
Name of the herbarium. 
}
  \item{HERBARIUM_CODE_ARGUMENT}{
Code of the herbarium.
}
  \item{TITLE_ARGUMENT}{
Title of the collection
}
  \item{PROJECT_ARGUMENT}{
Project the specimen related with
}
  \item{IDENTIFIED_BY_ARGUMENT}{
Name of the people who identified the specimen. 
}
  \item{DATE_IDENTIFIED_ARGUMENT}{
Date of the identification
}
  \item{PROCESSED_BY_ARGUMENT}{
Name of people proccessed the dataset. 
}
  \item{DATE_LASTMODIFIED_ARGUMENT}{
Date of the update. 
}
}
\details{
Columns names of the KFBG field book

1.COLL_NUMBER: collector's Number

2.COLL_DATE: collection date

3.COLL_TEAM: name of the collector and the people assisted the collection.

4.FIELD_ID_GENUS: genus

5.FIELD_ID_SP: specific epithet 

6.FIELD_ID_AUTHOR: authority of the species

7.FIELD_ID_INFRA_RANK: rank of the species, either: "var.", "f." or "subsp."

8.FIELD_ID_INFRA_SP: epithet for the infra species rank.

9.FIELD_ID_INFRA_AUTHOR: authority of the epithet. 

10.FAMILY: family name, preferably in APGIII system. 

11.PLANT_DESCRIPTION: description of the plant. 

12.LOCALITY: precise location where the specimens collected

13.REMARKS: association species and the description of the environment. 

14.ALTITUDE: altitude in meters. 

15.LAT_DEGREE: degree of the latitude

16.LAT_MINUTE: minute of the latitude

17.LAT_SECOND: second of the latitude

18.LAT_FLAG: direction of the latitude, either N or S. 

19.LON_DEGREE: degree of longitude. 

20.LON_MINUTE: minutes of longitude. 

21.LON_SECOND: seconds of longitude. 

22.LON_FLAG: direction of longitude, either E or W. 

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
\code{\link{scbg_fb2ht}},  \code{\link{kfbg_new_db2ht}} , \code{\link{kfbg_existing_db2ht}}
}
\examples{

bbsp_csv_file <- system.file("extdata", "KFBG_FIELDBOOK.csv", 
                             package = "herblabel")
kfbg_fb2ht(bbsp_csv_file)

}

\keyword{ format }
\keyword{ conversion }
\keyword{ template }
