
library(data.table)
library(readxl)

TobList <- readxl::read_excel("vignettes/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx", sheet = "Tobacco")
tob_disease_names <- as.character(c(unique(TobList$condition)))

usethis::use_data(tob_disease_names, overwrite = T)



AlcList <- readxl::read_excel("vignettes/16102018tobaccoandalcoholDiseaseListandRiskFunctions.xlsx", sheet = "Alcohol")
alc_disease_names <- as.character(c(unique(AlcList$condition)))



usethis::use_data(alc_disease_names, overwrite = T)
