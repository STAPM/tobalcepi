
# This code reads and processes the relative risks for tobacco
# They are stored in a marker file
# This code reads that file and cleans it to prepare the data to be used in the model

library(readxl)

# Load the master spreadsheet containing disease risks
tobacco_relative_risks <- readxl::read_excel("vignettes/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx",
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
