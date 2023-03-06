
# The aim of this code is to provide a map of single diseases into groups

library(data.table)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "X:/"

disease_groups <- fread("vignettes/inst/disease_groups.csv")

usethis::use_data(disease_groups, overwrite = T)
