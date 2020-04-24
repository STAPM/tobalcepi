
library(data.table)
library(readxl)

TobList <- read_excel("/Volumes/shared/ScHARR/PR_Disease_Risk_TA/Disease_Lists/16102018 tobacco and alcohol Disease List and Risk Functions.xlsx", sheet = "Tobacco")
tob_disease_names <- as.character(c(unique(TobList$condition)))

usethis::use_data(tob_disease_names, overwrite = T)



AlcList <- read_excel("/Volumes/shared/ScHARR/PR_Disease_Risk_TA/Disease_Lists/16102018 tobacco and alcohol Disease List and Risk Functions.xlsx", sheet = "Alcohol")
alc_disease_names <- as.character(c(unique(AlcList$condition)))

usethis::use_data(alc_disease_names, overwrite = T)
