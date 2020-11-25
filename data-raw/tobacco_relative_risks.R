
# This code reads and processes the relative risks for tobacco
# for current vs. never smokers

# They are stored in a master spreadsheet in X:/ScHARR/PR_Disease_Risk_TA/Code/tables
# `16102018 tobacco and alcohol Disease List and Risk Functions.xlsx`

# This code reads that file and cleans it to prepare the data to be used in the model

library(readxl)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/"

# Load the master spreadsheet containing disease risks
tobacco_relative_risks <- readxl::read_excel(paste0(root_dir, 
  "ScHARR/PR_Disease_Risk_TA/Code/tables/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx"), 
  sheet = "Tobacco")

# Set it as a data table
data.table::setDT(tobacco_relative_risks)

# Select the versions marked as current
# and select the required columns
tobacco_relative_risks <- tobacco_relative_risks[Version == "Current", c("condition", "age", "sex", "Current")]

# Change the names
data.table::setnames(tobacco_relative_risks, "Current", "relative_risk")

# Save the result to the package data folder
usethis::use_data(tobacco_relative_risks, overwrite = T)

# Save a separate list of the disease names
tob_disease_names <- as.character(c(unique(tobacco_relative_risks$condition)))

usethis::use_data(tob_disease_names, overwrite = T)

