## Welcome to the Homepage of R Package herblabel

### What is it?

It is an R package helps to create and check RTF herbarium labels, annotation labels, based on the Darwin Core Format. Including the following functionalities: 

1. Parse a scientific name, and automatically fill the relevant fields including, FAMILY (in APGIII classification system), GENUS, SPECIES. 

2. Check the validity based on The Plant List Accepted Species Database/Flora of China. 

3. Number of conversion utilities to create the template.

4. Identify, lighlight and change the font to Italic for Latin Words. 

5. Its output is RTF, which could be opened and viewed in MS Word or Libre Office etc.

### How to Install: 

```R
library(devtools)
install_github("helixcn/herblabel")
```

If "devtools" has not been installed, install it by typing: 

```R
install.packages("devtools")
```

### How to use: 
1. herbarium labels:
    ```R
    library(herblabel)
    
    ### Path of the template csv file
    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.csv", package = "herblabel")
    ### infile argument receives Darwin Core Template CSV file
    herbarium_label(infile = path,  outfile = "HERBARIUM_LABELS.rtf")
    
    dat_test <- read.csv(path)
    ### dat argument receives  Darwin Core Template data.frame 
    herbarium_label(dat = dat_test, outfile = "HERBARIUM_LABELS_dat.rtf")
    ```
2. Annotation Labels

    ```R
    path <- system.file("extdata", "ANNOTATION_TEMPLATE.csv", package = "herblabel")
    annotation_label(infile = path)
    ```
    
Please feel free to send an email to **Jinlong Zhang** <jinlongzhang01@gmail.com> if you have any questions on how to use this package.
