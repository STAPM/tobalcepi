
library(data.table)

disease_groups <- fread("data-raw/Relative risks/disease_groups.csv")

usethis::use_data(disease_groups, overwrite = T)
