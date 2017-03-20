## Welcome to the Homepage of R Package herblabel

### What is it?

It is a R package for creating herbarium labels, based on Darwin Core Template. Including the following functionalities: 

1. Parsing scientific names, and filling the relevant fields, including: FAMILY, GENUS, SPECIES etc.

2. Checking the spelling based on The Plant List Database, Florae Reipublicae Popularis Sinicae (FRPS), Flora of China. 

### How to Install: 
herblabel is available on R-forge, and you can use the following command to install: 

```R
install.packages("herblabel", repos="http://R-Forge.R-project.org")
```
This packages depends on "openxlsx"
```R
install.packages("openxlsx")
```
Usually the package "Rcpp" which "openxlsx" depends on should be installed automatically. If not, please type to following command to install Rcpp:
```R
install.packages("Rcpp")
```
The Rtools tool chain should also be installed and well configured.
"https://cran.r-project.org/bin/windows/Rtools/"

For more information, please refer to (in Chinese): [http://blog.sciencenet.cn/blog-255662-849868.html](http://blog.sciencenet.cn/blog-255662-849868.html)

### How to use: 
1. Generating herbarium labels

    ```R
    library(openxlsx)
    library(herblabel)
    path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS.xlsx", 
                        package = "herblabel")
    dat <- read.xlsx(path)
    herbarium_label(dat, theme = "KFBG", outfile = "herbarium_labels_KFBG.rtf")  ### KFBG Style
    herbarium_label(dat, theme = "PE",   outfile = "herbarium_labels_PE.rtf")    ### PE Style
    herbarium_label(dat, theme = "KUN",  outfile = "herbarium_labels_KUN.rtf")   ### KUN Style
    herbarium_label(dat, theme = "HU",   outfile = "herbarium_labels_HU.rtf")    ### Harvard University
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
The R script "run_herblabel" helps you to generate herbarium labels by a simple click. See [https://github.com/helixcn/run_herblabel](https://github.com/helixcn/run_herblabel)

### Citation:
Jinlong Zhang, Huiling Zhu, Jingang Liu, Gunter A. Fischer. (2016). Principles behind designing herbarium specimen labels and the R package 'herblabel'. Biodiversity Science, 24(12): 1345-1352 (DOI: 10.17520/biods.2016230) 

Please feel free to send an email to **Jinlong Zhang** <jinlongzhang01@gmail.com> if you have any questions.
