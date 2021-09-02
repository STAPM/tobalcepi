
# The aim of this code is to provide a map of single diseases into groups

library(data.table)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/Shared/"

disease_groups <- fread(paste0(root_dir, "ScHARR/PR_Disease_Risk_TA/Code/tables/disease_groups.csv"))

usethis::use_data(disease_groups, overwrite = T)
