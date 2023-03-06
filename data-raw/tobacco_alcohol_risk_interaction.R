
# This code reads and processes the estimates for the effect that consumption of
# both tobacco and alcohol has for the risk of certain diseases

# They are stored in X:/ScHARR/PR_Disease_Risk_TA/Code/tables

# This code reads that file and cleans it to prepare the data to be used in the model


library(readxl)
library(data.table)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "X:/"

# Load the spreadsheet containing disease risks
tob_alc_risk_int <- data.table::fread("vignettes/inst/tob_alc_interactions_180119.csv")

# Select the versions marked as current
tob_alc_risk_int <- tob_alc_risk_int[Version == "Current"]

# Save the result to the package data folder
usethis::use_data(tob_alc_risk_int, overwrite = T)




##########################################
# Tobacco and alcohol related diseases

# Alcohol lookups
AlcList <- readxl::read_excel("vignettes/inst/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx", 
                              sheet = "Alcohol")
data.table::setDT(AlcList)
setnames(AlcList, colnames(AlcList), tolower(colnames(AlcList)))
alc_icd10_lookups <- AlcList[ , c("condition", "icd10_lookups")]
alc_icd10_lookups[stringr::str_detect(condition, "Oesoph"), condition := "Oesophageal"]
alc_icd10_lookups <- unique(alc_icd10_lookups, by = "icd10_lookups")


# Tobacco lookups
tob_rr_data <- readxl::read_excel("vignettes/inst/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx", 
                                  sheet = "Tobacco")
data.table::setDT(tob_rr_data)
setnames(tob_rr_data, colnames(tob_rr_data), tolower(colnames(tob_rr_data)))
tob_icd10_lookups <- tob_rr_data[version == "Current", c("condition", "icd10_lookups")]
tob_icd10_lookups[stringr::str_detect(condition, "Oesoph"), condition := "Oesophageal"]
tob_icd10_lookups <- unique(tob_icd10_lookups, by = "icd10_lookups")

# Merge the lookups
tobalc_icd10_lookups <- rbindlist(list(tob_icd10_lookups, alc_icd10_lookups))
tobalc_icd10_lookups <- unique(tobalc_icd10_lookups, by = "icd10_lookups")


usethis::use_data(tobalc_icd10_lookups, overwrite = T)




