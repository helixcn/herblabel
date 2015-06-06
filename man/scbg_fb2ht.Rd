\name{scbg_fb2ht}
\alias{scbg_fb2ht}

\title{
Converting the South China Botanic Garden field collection format to Darwin Core herbarium records template in CSV
}
\description{
Converting the South China Botanic Garden field collection format to Darwin Core herblabel template in CSV. 
}
\usage{
scbg_fb2ht(infile, outfile = "herblabel_temp.csv", 
HERBARIUM_ARGUMENT = "Kadoorie Farm and Botanic Garden Herbarium", 
HERBARIUM_CODE_ARGUMENT = "KFBG", TITLE_ARGUMENT = NA, 
PROJECT_ARGUMENT = NA, 
IDENTIFIED_BY_ARGUMENT = NA, 
DATE_IDENTIFIED_ARGUMENT = NA, 
PROCESSED_BY_ARGUMENT = NA, 
DATE_LASTMODIFIED_ARGUMENT = NA)
}

\arguments{
  \item{infile}{
The name of the kfbg_existing records in Character string.
}
  \item{outfile}{
The name of the csv file to be generated.
}
  \item{HERBARIUM_ARGUMENT}{
The title of the herbarium to be added to the generated template. 
}
  \item{HERBARIUM_CODE_ARGUMENT}{
The code of the herbarium to be added to the generated template. 
}
  \item{TITLE_ARGUMENT}{
The title of the collection to be added to the herbarium labels in the generated template. 
}
  \item{PROJECT_ARGUMENT}{
The project of the specimen related with. 
}
  \item{IDENTIFIED_BY_ARGUMENT}{
The name of the person who identified the specimen. 
}
  \item{DATE_IDENTIFIED_ARGUMENT}{
The date of the identification. 
}
  \item{PROCESSED_BY_ARGUMENT}{
The name of the person who processed the record. 
}
  \item{DATE_LASTMODIFIED_ARGUMENT}{
The date of last update. 
}
}

\details{
The template of South China Botanic Garden Collection Notebook has the following columns. 

1.COLLECTOR_NUMBER: The collector's number

2.VOLUMN: volumn of the record. 

3.COLLECTOR: name of the collector

4.DATE_COLLECTED: Date collected.

5.LOCALITY: precise location where the specimen collected.

6.ELEVATION: elevation in meters

7.TOPOLOGY: description of topology
8.REMARKS_1: Environment
9.DISTRIBUTION: Distribution of the species
10.ATTRIBUTES_1: Growth form
11.ATTRIBUTES_2: Bark
12.ATTRIBUTES_3: leaves

13.ATTRIBUTES_4: flower
14.ATTRIBUTES_5: fruit

15.USAGE: usage of the plant

16.REMARKS_2: additional notes

17.REPLICATES: number of copies

18.SEEDLINGS: notes on seedlings 

19.SEEDS: notes on seeds

20.SPIRIT_SAMPLES: whether spirit samples was collected.
21.LOCAL_NAME: Chinese Name

22.FAMILY: Family
23.SCIENTIFIC_NAME: Scientific Name of the species
24.IDENTIFIED_BY: Name who identified the plant. 
25.DATE_IDENTIFIED: Date of the identification
26.GENUS: Genus 
27.SPECIES: species
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
\code{\link{kfbg_fb2ht}}, \code{\link{kfbg_new_db2ht}} , \code{\link{kfbg_existing_db2ht}}
}

\examples{

infile <- system.file("extdata", "SCBG_FB_TEMPLATE.csv", 
                             package = "herblabel")
                             
scbg_fb2ht(infile)

}

\keyword{ format }
\keyword{ conversion }
\keyword{ template }
