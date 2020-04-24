
# This code reads and processes the estimates for the effect that consumption of
# both tobacco and alcohol has for the risk of certain diseases

# Load the spreadsheet containing disease risks
tob_alc_risk_int <- data.table::fread("X:/ScHARR/PR_Disease_Risk_TA/Disease_Lists/tob_alc_interactions_180119.csv")

# Select the versions marked as current
tob_alc_risk_int <- tob_alc_risk_int[Version == "Current"]

# Save the result to the package data folder
usethis::use_data(tob_alc_risk_int, overwrite = T)
