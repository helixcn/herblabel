\name{ht2herblabel_rtf}
\alias{ht2herblabel_rtf}
\title{
Creating RTF herbarium labels in Latex based on Darwin Core herbarium records template in CSV
}
\description{
Creating RTF herbarium labels in Latex based on Darwin Core herbarium records template in CSV.
}
\usage{
ht2herblabel_rtf(infile = NULL, spellcheck = TRUE, outfile = "herblabel.rtf")
}

\arguments{
  \item{infile}{
Character representing the template file in csv.
}
  \item{spellcheck}{ 
Logical, indicating whether the spell check will be performed.
}
  \item{outfile}{
the output rtf file.
}
}
\details{
This function reads Darwin Core herbarium records template in CSV, and generates a RTF file containing 
herbarium labels. Only a limited number of the columns can be left blank. 
This function was inspired The Plabel program developed by Kent D. Perkins at the University of 
Florida Herbarium.

\url{ http://www.flmnh.ufl.edu/natsci/herbarium/pl/ }


Columns of the template:

1	GLOBAL_UNIQUE_IDENTIFIER: Institution code plus the number

2	INSTITUTION_CODE: Code for the herbarium

3	COLLECTION_CODE: the collection code for the herbarium specimen, eg. the barcode number

4	BASIS_OF_RECORD: type of the specimen, eg. herbarium specimen

5	PREPARATIONS:fruiting, flowering or vegetative

6	HERBARIUM:Name of the herbarium

7	TITLE:Title of the collection, eg. Plants of Hong Kong

8	COLLECTOR:Full Name of the collector

9	ADDITIONAL_COLLECTOR:people assisted the collection

10	COLLECTOR_NUMBER:collector's number

11	DATE_COLLECTED:2013_4_11

12	LOCAL_NAME:local name for this plant

13	FAMILY:APGIII Family

14	GENUS:genus

15	SPECIES:specific epithetic

16	AUTHOR_OF_SPECIES:author

17	INFRASPECIFIC_RANK:"var.", "f.", or "subsp."

18	INFRASPECIFIC_EPITHET:infraspecific epithet

19	AUTHOR_OF_INFRASPECIFIC_RANK:author of the infraspecific epithet

20	COUNTRY:country of the specimen collected

21	STATE_PROVINCE:state or province 

22	COUNTY: county

23	LOCALITY: precise locality

24	LOCALITY_ORIGINAL: locality in local language

25	IMAGE_URL: url for the image

26	RELATED_INFORMATION: the reference of the name

27	LAT_DEGREE:latitude, degrees

28	LAT_MINUTE:latitude, minutes

29	LAT_SECOND:latitude, seconds

30	LAT_FLAG:N for northern hemisphere, S for southern hemisphere

31	LON_DEGREE:longitude, degrees

32	LON_MINUTE:longitude, minutes

33	LON_SECOND:longitude, seconds

34	LON_FLAG:E for eastern hemisphere, W for western hemisphere

35	ELEVATION:elevation in meters

36	ATTRIBUTES:description of the species

37	REMARKS:the associated species, and the description of habitats

38	CABINET:location of the specimen

39	DISPOSITION:in herbarium, 

40	GEOREFERENCE_SOURCES:GPS or Google Earth

41	GEOREFERENCE_VERIFICATION_STATUS:YES

42	GEOREFERENCE_REMARKS:accuracy for the georeference

43	PROJECT:Project the specimen related with

44	IDENTIFIED_BY: who did the identification

45	DATE_IDENTIFIED: date of identification

46	TYPE_STATUS:if not type, 

47	PROCESSED_BY: data entered by

48	DATE_LASTMODIFIED: date of the modification

If spellcheck is TRUE, the text will be highlighted if the genus does not match the family. 
Comments will be automatically generated. If you do not want to change 
the information, just suppress it by changing "spellcheck = FALSE". 

}

\seealso{
\code{\link{ht2herblabel_tex}}
}


\author{
Jinlong Zhang 
}

\examples{

    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.csv", 
                        package = "herblabel")
    ht2herblabel_rtf(infile = path,  
                  outfile = "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE_ht2herblabel_rtf.rtf")
    
}
