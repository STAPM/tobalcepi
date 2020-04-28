
library(data.table)

disease_groups <- fread("vignettes/disease_groups.csv")

usethis::use_data(disease_groups, overwrite = T)
