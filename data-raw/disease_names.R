
# This code stores the list of the names of diseases that are attributable to tobacco and alcohol

# doing this helps to keep the formatting of disease names consistent

library(data.table)
library(readxl)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/"

# Load the master spreadsheet containing disease risks
TobList <- readxl::read_excel(paste0(root_dir, "ScHARR/PR_Disease_Risk_TA/Code/tables/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx"), sheet = "Tobacco")

tob_disease_names <- as.character(c(unique(TobList$condition)))

usethis::use_data(tob_disease_names, overwrite = T)



AlcList <- readxl::read_excel(paste0(root_dir, "ScHARR/PR_Disease_Risk_TA/Code/tables/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx"), sheet = "Alcohol")

alc_disease_names <- as.character(c(unique(AlcList$condition)))


usethis::use_data(alc_disease_names, overwrite = T)
