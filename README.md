## Welcome to the Homepage of R Package herblabel

### What is it?

It is an R package to create and check herbarium specimen labels and annotation labels, based on the Darwin Core Template. Including the following functionalities: 

1. Parsing a scientific name, and filling the relevant fields automatically, including: FAMILY (according to the Plantlist Website), GENUS, SPECIES. 

2. Checking the validity based on The Plant List Accepted Species Database/Flora Reipublicae Popularis Sinicae (FRPS). 

3. Identify and change the Latin Words to Italic in remarks field. 

### How to Install: 
herblabel depends on the R package openxlsx. 
to install openxlsx: 

```R
library(devtools)
install_github("/awalker89/openxlsx")
```

To install herblabel: 
```R
library(devtools)
install_github("helixcn/herblabel")
```

If "devtools" has not been installed, please install it by typing: 

```R
install.packages("devtools")
```

### How to use: 
1. herbarium labels

    ```R
    library(openxlsx)
    library(herblabel)
    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS.xlsx", 
                        package = "herblabel")
    dat <- read.xlsx(path)
    herbarium_label(dat, theme = "KFBG", outfile = "herbarium_labels_KFBG.rtf")
    herbarium_label(dat, theme = "PE",   outfile = "herbarium_labels_PE.rtf")
    herbarium_label(dat, theme = "KUN",  outfile = "herbarium_labels_KUN.rtf")
    herbarium_label(dat, theme = "HU",   outfile = "herbarium_labels_HU.rtf")
    herbarium_label(dat, spellcheck = FALSE, outfile = "herbarium_labels_no_checking.rtf")
    ```
2. Annotation Labels

    ```R
    library(herblabel)
    
    library(openxlsx)
    
    path <- system.file("extdata", "ANNOTATION_TEMPLATE.xlsx", 
                       package = "herblabel")
                       
    dat <- read.xlsx(path)
    
    annotation_label(dat)
    ```
    
Please feel free to send an email to **Jinlong Zhang** <jinlongzhang01@gmail.com> if you have any questions on how to use this package.

