\name{kfbg_bgbase_csv2ht}
\alias{kfbg_bgbase_csv2ht}
\title{
BG-BASE SPECIMENS table Conversion
}
\description{
Convert the exported BG-BASE SPECIMENS CSV to herblabel template
}
\usage{
kfbg_bgbase_csv2ht(infile, outfile = NULL, 
HERBARIUM = "Kadoorie Farm and Botanic Garden Herbarium", 
HERBARIUM_CODE = "KFBG", TITLE = NA, 
PROJECT = NA, PROCESSED_BY = NA, DATE_LASTMODIFIED = NA)
}

\arguments{
  \item{infile}{
The csv file exported from the SPECIMENS table by slist of BG-BASE
}
  \item{outfile}{
The output csv file.
}
  \item{HERBARIUM}{
Name of the herbarium
}
  \item{HERBARIUM_CODE}{
Code of the herbarium
}
  \item{TITLE}{
Plants of some place
}
  \item{PROJECT}{
The project the specimen related with
}
  \item{PROCESSED_BY}{
The name of the person that makes the conversion
}
  \item{DATE_LASTMODIFIED}{
The date of the conversion
}
}
\details{
Many Botanic Gardens use the BG-BASE to manage the herbarium specimens. 
All the specimen records will go to the SPECIMENS table in BG-BASE. 
The records itself  has 771 fields in the SPECIMENS table, of which 
191 are core fields, eg. Collector, Collector's Number. 

Exporting the records from BG-BASE specimens can be achieved by sending
 the command to slist interface. None ASCII character will result lead 
to corrupted files. 

The relationship between the fields in SPECIMENS table, the exported file 
from SPECIMENS table and the herblabel template could be found at the 
extdata folder of this package. The fields not included in the SPECIMENS 
table but the herblabel template needs should be filled either by manually
editing or by specifying during the conversion (see the arguments).  

}
\value{
The data frame
}
\references{
BG-BASE http://www.bg-base.com/
Relationship between the BG-BASE SPECIMENS TABLE and the herblabel 
template.xlsx
}
\author{
Jinlong Zhang
}

\examples{
bbsp_csv_file <- system.file("extdata", "EXPORT20150121.CSV", 
                             package = "herblabel")
kfbg_bgbase_csv2ht(infile = bbsp_csv_file,  outfile = "herblabel.csv")
}
