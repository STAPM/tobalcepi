
# This code reads and processes the lag times for tobacco

# They are stored in X:/ScHARR/PR_Disease_Risk_TA/Code/tables

# This code reads that file and cleans it to prepare the data to be used in the model

library(data.table)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/"

# Load the spreadsheet containing the lag times
# These are taken from Kontis et al. 2015 and were sent to us by the author
tobacco_lag_times <- data.table::fread(paste0(root_dir, "ScHARR/PR_Disease_Risk_TA/Code/tables/excess_risk_decline_from_KontisLancet.csv"))

# Select the versions marked as current
# and select the required columns
tobacco_lag_times <- tobacco_lag_times[years_since_cessation %in% 0:40, c("cause_group", "years_since_cessation", "excess_risk_percent")]

# Change the names
setnames(tobacco_lag_times, "years_since_cessation", "time_since_quit")

# Save the result to the package data folder
usethis::use_data(tobacco_lag_times, overwrite = T)
