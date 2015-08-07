#####################################################################################
############### R script for generating herbarium specimen labels ###################
##################### using herblabel package #######################################
##################### Jinlong Zhang #################################################
##################### Flora Conservation Department #################################
##################### Kadoorie Farm and Botanic Garden, Hong Kong SAR. ##############

### Put your templates excel file or csv file in the folder.
### Change the working directory
setwd("C:\\Users\\documents") ### if using Windows

### Load required libraries
library(openxlsx) ### library for Reading Excel 2007 or later
library(herblabel) ### load the herbarium label package

### Read data from the Excel Template
dat2 <- read.xlsx(xlsxFile = "KFBG_herbarium_records_template_20150807.xlsx")

### convert the dataframe from the dataframe. 
ht2herblabel_rtf(dat = dat2, outfile = "HERBARIUM_LABELS_20150807_from_Excel.rtf")

### convert from CSV file. 
ht2herblabel_rtf(infile = "KFBG_herbarium_records_template_20150807.csv",  
                 outfile = "HERBARIUM_LABELS_from_CSV.rtf")
