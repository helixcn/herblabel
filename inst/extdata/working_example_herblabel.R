#####################################################################################
############### R script for generating herbarium specimen labels ###################
##################### using herblabel package #######################################
##################### Jinlong Zhang #################################################
##################### Flora Conservation Department #################################
##################### Kadoorie Farm and Botanic Garden, Hong Kong SAR. ##############

### Put your templates excel file or csv file in the folder.
### Change the working directory

working_dir <- system.file("extdata", package = "herblabel")
setwd(working_dir)

### Load required libraries
library(openxlsx)  ### library for Reading Excel 2007 or later
library(herblabel) ### load the herbarium label package

### Read data from the Excel Template
dat2 <- read.xlsx(xlsxFile = "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.xlsx")

### (1) Making herbarium labels 
### Make herbarium labels from dataframe in Darwin Core Format. 
herbarium_label(dat = dat2, outfile = "output_HERBARIUM_LABELS_from_data_frame.rtf")

### Make herbarium labels from CSV file in Darwin Core Format. 
herbarium_label(infile = "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.csv",  
                 outfile = "output_HERBARIUM_LABELS_from_CSV.rtf")

### (2) Making Compact Herbarium labels
### Making compact herbarium labels from dataframe in Darwin Core Format.
compact_label(dat = dat2, outfile = "output_Compact_HERBARIUM_LABELS_from_Excel.rtf")

### Make herbarium labels from CSV file in Darwin Core Format. 
compact_label(infile = "DARWIN_CORE_HERBARIUM_RECORDS_TEMPLATE.csv",  
              outfile = "output_Compact_HERBARIUM_LABELS_from_CSV.rtf")

### (3) Making Annotation Labels 
annotation_label(infile = "ANNOTATION_TEMPLATE.csv", 
                 outfile = "output_Annotation_labels_csv.rtf")

dat_annotation <- read.csv("ANNOTATION_TEMPLATE.csv", header = TRUE)
annotation_label(dat = dat_annotation, outfile = "output_Annotation_labels_data_frame.rtf")
                 
### (4) Converting Different Format to Darwin Core Format
### Convert the database exported from BG-BASE to Darwin Core Format 
bgbase_csv2ht(infile = "BG_BASE_EXPORT20150121.CSV",  outfile = "output_Darwin_Core_herblabel_template_BG_BASE.csv")

### Convert KFBG existing specimen dataset to Darwin Core Format 
kfbg_existing_db2ht(infile = "KFBG_EXISTING_DB.csv", outfile = "output_Darwin_Core_herblabel_template_KFBG_EXISTING_DB.csv")

### Convert KFBG new database to Darwin Core Format
kfbg_new_db2ht(infile = "KFBG_NEW_DB.csv", outfile = "output_Darwin_Core_herblabel_template_KFBG_NEW_DB.csv")

### Convert KFBG field book to Darwin Core Format
kfbg_fb2ht(infile = "KFBG_FIELDBOOK.csv", outfile = "output_Darwin_Core_herblabel_template_KFBG_fieldbook.csv")

### Convert Collection Records from South China Botanic Garden to Darwin Core Format
scbg_fb2ht(infile = "SCBG_FB_TEMPLATE.csv", outfile = "output_Darwin_Core_herblabel_template_SCBG_FB_TEMPLATE.csv")

               
### delete the RTF files
unlink("output_HERBARIUM_LABELS_from_data_frame.rtf")
unlink("output_HERBARIUM_LABELS_from_CSV.rtf")
unlink("output_Compact_HERBARIUM_LABELS_from_Excel.rtf")
unlink("output_Compact_HERBARIUM_LABELS_from_CSV.rtf")
unlink("output_Annotation_labels_csv.rtf")
unlink("output_Annotation_labels_data_frame.rtf")
unlink("output_Darwin_Core_herblabel_template_BG_BASE.csv")
unlink("output_Darwin_Core_herblabel_template_KFBG_EXISTING_DB.csv")
unlink("output_Darwin_Core_herblabel_template_KFBG_NEW_DB.csv")
unlink("output_Darwin_Core_herblabel_template_KFBG_fieldbook.csv")
unlink("output_Darwin_Core_herblabel_template_SCBG_FB_TEMPLATE.csv")

