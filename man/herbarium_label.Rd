\name{herbarium_label}
\alias{herbarium_label}
\title{
Creating RTF herbarium labels based on Darwin Core herbarium records template
}
\description{
Creating RTF herbarium labels based on Darwin Core herbarium records template.
}
\usage{
herbarium_label(dat = NULL, spellcheck = TRUE, theme = c("KFBG", "PE", "KUN", "HU"), 
                outfile = "herblabel.rtf", font = c("Roman","Arial"), font_size = 1)
}

\arguments{
   \item{dat}{
dataframe of Darwin Core Template.  
}
  \item{spellcheck}{ 
Logical, indicating whether the check of spelling should be performed.
}

  \item{theme}{ 
a character string speficying the theme of herbarium labels
}

  \item{outfile}{
Character string, name of the rtf file to be generated.
}

  \item{font}{
  Font for all the labels to be generated,either "Roman" for proportionally spaced serif fonts or "Arial", proportionally spaced sans serif fonts respectively.  
}

  \item{font_size}{
  Relative Font Size, by default it is 1. 
}

}
\details{
This function reads Darwin Core herbarium records template in CSV, and generates a RTF file. It will check the completeness for each entry: Labels will not generated if some critical fields is missing, eg. Collector, Collector's Number. Therefore, a specimen without collector's number, the collector's number should be marked as "s.n.". 

Please follow the APGIII families for all the species, if mismatch is found, there will be a warning in the RTF label. 

The validity of the species will be checked based on the Plant List Website \url{http://www.theplantlist.org/ } or Flora of China \url{http://frps.eflora.cn/ }. So make sure the species is spelled correctly, and the Abbreviation of Authors is correctly spelled. 

The function will also identify and highlight the latin words in the Remarks field. 

Details for the columns of the template. please look at the file "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.xlsx" in the extdata folder:

1. \code{GLOBAL_UNIQUE_IDENTIFIER}: Institution code plus the number, eg. KFBG000102

2. \code{INSTITUTION_CODE}: Code for the herbarium, eg. KFBG

3. \code{COLLECTION_CODE}: the collection code for the herbarium specimen, eg. the barcode number

4. \code{BASIS_OF_RECORD}: type of the specimen, eg. herbarium specimen

5. \code{PREPARATIONS}:fruiting, flowering or vegetative, eg. fruiting

6. \code{HERBARIUM}:Name of the herbarium, eg. Kadoorie Farm and Botanic Garden Herbarium (KFBG).

7. \code{TITLE}: Title of the collection, eg. Plants of Hong Kong

8. \code{COLLECTOR}:Full Name of the collector.

9. \code{ADDITIONAL_COLLECTOR}:People assisted the collection.

10. \code{COLLECTOR_NUMBER}:Collector's number

11. \code{DATE_COLLECTED}:eg. Must must follow the YYYY-MM-DD format, eg. 2013-4-11, representing 11 April 2013.

12. \code{LOCAL_NAME}:Local name for this plant.

13. \code{FAMILY}:APGIII Family

14. \code{GENUS}:Genus

15. \code{SPECIES}:specific epithetic

16. \code{AUTHOR_OF_SPECIES}:author

17. \code{INFRASPECIFIC_RANK}: Infra-specific rank, one of "var.", "f.", or "subsp."

18. \code{INFRASPECIFIC_EPITHET}:infraspecific epithet

19. \code{AUTHOR_OF_INFRASPECIFIC_RANK}:author of the infraspecific epithet

20. \code{COUNTRY}:country of the specimen collected, must be provided. 

21. \code{STATE_PROVINCE}:state or province, must be provided.

22. \code{COUNTY}: county

23. \code{LOCALITY}: precise locality

24. \code{LOCALITY_ORIGINAL}: locality in local language.

25. \code{IMAGE_URL}: url for the image of this specimen. 

26. \code{RELATED_INFORMATION}: the reference of the name

27. \code{LAT_DEGREE}:latitude, degrees, in WGS1984 geocoordinate system.

28. \code{LAT_MINUTE}:latitude, minutes, in WGS1984 geocoordinate system.

29. \code{LAT_SECOND}:latitude, seconds, in WGS1984 geocoordinate system.

30. \code{LAT_FLAG}:N for northern hemisphere, S for southern hemisphere

31. \code{LON_DEGREE}:longitude, degrees, in WGS1984 geocoordinate system.

32. \code{LON_MINUTE}:longitude, minutes, in WGS1984 geocoordinate system.

33. \code{LON_SECOND}:longitude, seconds, in WGS1984 geocoordinate system.

34. \code{LON_FLAG}:E for eastern hemisphere, W for western hemisphere

35. \code{ELEVATION}:elevation in meters, please only provide the number. 

36. \code{ATTRIBUTES}:description of the species. eg. Tree, 8m tall. Bark brownish. Young branchlet covered with dense hair. Leaf Underside greenish grey. Anthers yellow, petals red. Fruit 8-12mm, globose, yellow.  

37. \code{REMARKS}:the associated species, and the description of habitats. Eg. Understory, with Lasianthus fordii, Ardisia quinquegona etc.

38. \code{CABINET}:location of the specimen

39. \code{DISPOSITION}:eg. in herbarium 

40. \code{GEOREFERENCE_SOURCES}:GPS or Google Earth

41. \code{GEOREFERENCE_VERIFICATION_STATUS}:whether the georeferecing data has been veryfied or not. eg. YES

42. \code{GEOREFERENCE_REMARKS}:accuracy for the georeference

43. \code{PROJECT}:Project the specimen related with

44. \code{IDENTIFIED_BY}: who did the identification

45. \code{DATE_IDENTIFIED}: date of identification, eg. 2015-10-18

46. \code{TYPE_STATUS}:if the specimen is a type specimen or not. if yes, please specify which kind of type specimen it belong to. 

47. \code{PROCESSED_BY}: data entered by

48. \code{DATE_LASTMODIFIED}: date of the modification, eg. 2015-10-18

}

\references{
The Plabel program developed by Kent D. Perkins at the University of Florida Herbarium \url{ http://www.flmnh.ufl.edu/natsci/herbarium/pl/ }.
}

\author{
Jinlong Zhang \email{ jinlongzhang01@gmail.com }
}

\examples{
    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS.xlsx", 
                        package = "herblabel")
    library(openxlsx)
    dat <- read.xlsx(path)
    
    herbarium_label(dat, theme = "KFBG", outfile = "herbarium_labels_KFBG.rtf")
    herbarium_label(dat, theme = "PE",   outfile = "herbarium_labels_PE.rtf")
    herbarium_label(dat, theme = "KUN",  outfile = "herbarium_labels_KUN.rtf")
    herbarium_label(dat, theme = "HU",  outfile = "herbarium_labels_HU.rtf")
    herbarium_label(dat, spellcheck = FALSE, 
                    outfile = "herbarium_labels_no_checking.rtf")
    
    herbarium_label(dat, theme = "KFBG", outfile = "herbarium_labels_KFBG_1.1.rtf", font_size = 1.1)
    herbarium_label(dat, theme = "PE",   outfile = "herbarium_labels_PE_1.2.rtf"  , font_size = 1.2)
    herbarium_label(dat, theme = "KUN",  outfile = "herbarium_labels_KUN_0.8.rtf" , font_size = 0.8)
    herbarium_label(dat, theme = "HU",   outfile = "herbarium_labels_HU_0.8.rtf"  , font_size = 0.8)
    herbarium_label(dat, spellcheck = FALSE, 
                    outfile = "herbarium_labels_no_checking.rtf"              , font_size = 1.5)
    
    ##### Labels using Sans Serif Fonts
    herbarium_label(dat, theme = "KFBG", 
                    outfile = "herbarium_labels_KFBG_Arial.rtf", 
                    font = "Arial")
    herbarium_label(dat, theme = "PE",   
                    outfile = "herbarium_labels_PE_Arial.rtf", 
                    font = "Arial")
    herbarium_label(dat, theme = "KUN",  
                    outfile = "herbarium_labels_KUN_Arial.rtf", 
                    font = "Arial")
    herbarium_label(dat, theme = "HU",  
                    outfile = "herbarium_labels_HU_Arial.rtf", 
                    font = "Arial")
    herbarium_label(dat, spellcheck = FALSE, 
                    outfile = "herbarium_labels_no_checking_Arial.rtf", 
                    font = "Arial")
    
}
