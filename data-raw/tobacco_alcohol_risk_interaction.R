
# This code reads and processes the estimates for the effect that consumption of
# both tobacco and alcohol has for the risk of certain diseases

# They are stored in X:/ScHARR/PR_Disease_Risk_TA/Code/tables

# This code reads that file and cleans it to prepare the data to be used in the model

library(data.table)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/"

# Load the spreadsheet containing disease risks
tob_alc_risk_int <- data.table::fread(paste0(root_dir, "ScHARR/PR_Disease_Risk_TA/Code/tables/tob_alc_interactions_180119.csv"))

# Select the versions marked as current
tob_alc_risk_int <- tob_alc_risk_int[Version == "Current"]

# Save the result to the package data folder
usethis::use_data(tob_alc_risk_int, overwrite = T)
