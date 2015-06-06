\name{kfbg_existing_db2ht}
\alias{kfbg_existing_db2ht}
\title{
Convert the existing herbarium specimens records to Darwin Core herblabel template in CSV
}
\description{
Convert the KFBG existing herbarium specimen records to Darwin Core herblabel template in CSV. Both of the input and the output file are csv. 
}
\usage{

kfbg_existing_db2ht(infile, outfile = "herblabel_temp.csv", 
    HERBARIUM_ARGUMENT = "Kadoorie Farm and Botanic Garden Herbarium", 
    HERBARIUM_CODE_ARGUMENT = "KFBG", TITLE_ARGUMENT = NA, 
    PROJECT_ARGUMENT = NA, IDENTIFIED_BY_ARGUMENT = NA, 
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
The existing database of plant collection records in KFBG herbarium has the following column names: 

1. NUMBER: previous herbarium number. 

2. CURRENT_LOCATION: location of the specimen.

3. FAMILY: family

4. STORAGE_LOCATION: storage location of the specimen supposed to be. 

5. SCIENTIFIC_NAME: Scientific name

6. NOMEN_GENERARUM: Genus

7. EPITHETON_SPECIFICUM: specific epithetic

8. SCIENTIFIC_AUTHORITY_FOR_SPECIES: authority of the species

9. SUB-SPECIES_OR_VARIETY_OR_CULTIVATED_NAME: epithetic rank of the species

10. SCIENTIFIC_AUTHORITY_FOR_VARIETY: authority of the infra species rank

11. SYNONYMS: synomyms

12. ENGLISH_NAME: English Name

13. OTHER_ENGLISH_NAME: Other English Name

14. ACCEPTED_CHINESE_NAME: Chinese Name accepted in Flora of Hong Kong or Flora of China. 

15. OTHER_CHINESE_NAME: Other Chinese Names

16. FLOWERS: if the specimen has flower.

17. FRUITS: if the specimen has fruits.

18. LEAF: if the specimen has Leaf.

19. WHOLE: if the whole plant was taken as the specimen.

20. SPORANGIUM: if the specimen has Sporangia.

21. HERBARIUM_NUMBER: Herbarium number.

22. DATE_OF_ENTRY: when the specimen entered herbarium. 

23. VERIFIED_BY: people identified the plant.

24. VERIFICATION_DATE: date of the identification.

25. COLLECTOR: Collector's Name.

26. COLLECTOR_NUMBER: Collector's Number.

27. LOCATION_OF_COLLECTION_IN_ENGLISH: Location of the specimen collected in English. 

28. LOCATION_OF_COLLECTION_IN_CHINESE: Location of the specimen collected in Chinese. 

29. DATE_OF_COLLECTION: Date of the collection.

30. NUMBER_OF_SPECIMEN: Copies of the specimen.

31. QUANTITY_OF_SPECIMENS: Number of specimen.

32. ALTITUDE: Altitude.

33. LONGITUDE: Logitude.

34. LATITUDE: Latitude.

35. FIELD_NOTES: Field Notes.

36. RECORDED_IN_HK: Whether the species was recorded in Hong Kong.

37. TYPE_SPECIMEN: The type status of the specimen.

38. PHOTO: File name of the photo.

39. TYPE_OF_PHOTO_STORAGE: How the photo is stored. 

40. LINK_OF_PHOTO: Linkg to the photo.

41. FREEZING: Whether the specimen was freezed. 

42. DATE_OF_FREEZING: Date of freezing.

43. DATE_TO_TRANSITION: Date to the transition zone. 

44. DATE_OF_INVENTORY: Date of inventory.

45. CONDITIONS_OF_SPECIMEN: Condition of the specimen.

46. REMARKS: remarks for the specimen.

47. DATE_OF_DATA_ENTRY: Date of Data Entry.


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
\code{\link{kfbg_fb2ht}}, \code{\link{scbg_fb2ht}},  \code{\link{kfbg_new_db2ht}} 
}
\examples{

    path <- system.file("extdata", "KFBG_EXISTING_DB.csv", 
                        package = "herblabel")
                        
    kfbg_existing_db2ht(path)

}

\keyword{ format }
\keyword{ conversion }
\keyword{ template }
