
# This code reads and processes the lag times for tobacco

# Load the spreadsheet containing the lag times
# These are taken from Kontis et al. 2015 and were sent to us by the author
tobacco_lag_times <- data.table::fread("vignettes/excess_risk_decline_from_KontisLancet.csv")

# Select the versions marked as current
# and select the required columns
tobacco_lag_times <- tobacco_lag_times[years_since_cessation %in% 0:40, c("cause_group", "years_since_cessation", "excess_risk_percent")]

# Change the names
setnames(tobacco_lag_times, "years_since_cessation", "time_since_quit")

# Save the result to the package data folder
usethis::use_data(tobacco_lag_times, overwrite = T)
