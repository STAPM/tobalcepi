
# This code reads and processes the relative risks and disease definitions for tobacco

# They are stored in a master spreadsheet in X:/ScHARR/PR_Disease_Risk_TA/Code/tables
# `16102018 tobacco and alcohol Disease List and Risk Functions.xlsx`


library(readxl)
library(data.table)

##########################################

# Load the data 


# Set the file path to point to the University of Sheffield X drive
root_dir <- "X:/"

# Load the master spreadsheet containing disease risks
tob_rr_data <- readxl::read_excel("vignettes/inst/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx", 
  sheet = "Tobacco")

# Set it as a data table
data.table::setDT(tob_rr_data)

# Columns names to consistent lower case
setnames(tob_rr_data, colnames(tob_rr_data), tolower(colnames(tob_rr_data)))


##########################################

# Save relative risks

# Select the versions marked as current
# and select the required columns
tobacco_relative_risks <- tob_rr_data[version == "Current", c("condition", "age", "sex", "current")]

# Change the names
data.table::setnames(tobacco_relative_risks, "current", "relative_risk")

# Save the result to the package data folder
usethis::use_data(tobacco_relative_risks, overwrite = T)


##########################################

# Save disease names

# Save a separate list of the disease names
tob_disease_names <- as.character(c(unique(tobacco_relative_risks$condition)))

usethis::use_data(tob_disease_names, overwrite = T)


##########################################

# Save ICD-10 lookups

# The aim of this code is to prepare lists of tobacco and alcohol related diseases
# with their corresponding ICD-10 codes

# These data are used to search for the relevant conditions 
# in the process of preparing the mortality and hospitalisation rates
# for use in the modelling

tob_icd10_lookups <- tob_rr_data[version == "Current", c("condition", "icd10_lookups")]

tob_icd10_lookups[stringr::str_detect(condition, "Oesoph"), condition := "Oesophageal"]

tob_icd10_lookups <- unique(tob_icd10_lookups, by = "icd10_lookups")

usethis::use_data(tob_icd10_lookups, overwrite = T)







