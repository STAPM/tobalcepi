
# This code stores the list of the names of diseases that are attributable to tobacco and alcohol

# doing this helps to keep the formatting of disease names consistent

library(data.table)
library(readxl)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/"


##########################################

# Load the data 


AlcList <- readxl::read_excel("vignettes/inst/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx", 
  sheet = "Alcohol")

# Set it as a data table
data.table::setDT(AlcList)

# Columns names to consistent lower case
setnames(AlcList, colnames(AlcList), tolower(colnames(AlcList)))

##########################################

# Save disease names

alc_disease_names <- as.character(c(unique(AlcList$condition)))


usethis::use_data(alc_disease_names, overwrite = T)

##########################################

# Save ICD-10 lookups

# The aim of this code is to prepare lists of tobacco and alcohol related diseases
# with their corresponding ICD-10 codes

# These data are used to search for the relevant conditions 
# in the process of preparing the mortality and hospitalisation rates
# for use in the modelling


alc_icd10_lookups <- AlcList[ , c("condition", "icd10_lookups")]

alc_icd10_lookups[stringr::str_detect(condition, "Oesoph"), condition := "Oesophageal"]

alc_icd10_lookups <- unique(alc_icd10_lookups, by = "icd10_lookups")

usethis::use_data(alc_icd10_lookups, overwrite = T)



