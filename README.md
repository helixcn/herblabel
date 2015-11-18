## Welcome to the Homepage of R Package herblabel

It is able to prepare labels in RTF for herbarium specimens based on a templated in Darwin Core format. It helps you to check the Family, Genus relationship in APGIII classification system, and also checks the validity of Scientific Names based on The Plant List Website. 

To install herblabel, please enter:

```R
library(devtools)
install_github("helixcn/herblabel")`
```
in R console. 

If you haven't had devtools installed, please install it by typing 

```R
install.packages("devtools")` 
```
in R console.

How to use, please copy the following example to the R console:

```R
path <- system.file("extdata", "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.csv", 
                     package = "herblabel")
library(herblabel)
herbarium_label(infile = path,  
             outfile = "HERBARIUM_LABELS.rtf")
dat_test <- read.csv(path)
herbarium_label(dat = dat_test, outfile = "HERBARIUM_LABELS_dat.rtf")
```

if you have any comments, please feel free to send an email to **Jinlong Zhang** <jinlongzhang01@gmail.com> .
