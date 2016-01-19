## Welcome to the Homepage of R Package herblabel

### What is it?

It is an R package to create and check herbarium specimen labels and annotation labels, based on the Darwin Core Template. Including the following functionalities: 

1. Parsing a scientific name, and filling the relevant fields automatically, including: FAMILY (according to the Plantlist Website), GENUS, SPECIES. 

2. Checking the validity based on The Plant List Accepted Species Database/Flora Reipublicae Popularis Sinicae (FRPS). 

3. Identify and change the Latin Words to Italic in remarks field. 

### How to Install: 

```R
library(devtools)
install_github("helixcn/herblabel")
```

If "devtools" has not been installed, please install it by typing: 

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
