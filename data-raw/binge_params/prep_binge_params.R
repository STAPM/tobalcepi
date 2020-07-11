
# This code reads and prepares the parameters for estimating 
# the distribution of amount drunk on single drinking ocassions

# The parameters are from Hill-McManus et al 2014 
# Estimation of usual occasion-based individual drinking patterns using diary survey data
# https://doi.org/https://doi.org/10.1016/j.drugalcdep.2013.09.022. 

library(readxl)
library(data.table)
library(magrittr)

# Load the spreadsheet containing the parameters
binge_params_table3 <- readxl::read_excel("data-raw/binge_params/Hill-McManus_binge_params.xlsx", sheet = "Table 3") %>% setDT
binge_params_table5 <- readxl::read_excel("data-raw/binge_params/Hill-McManus_binge_params.xlsx", sheet = "Table 5") %>% setDT
binge_params_table6 <- readxl::read_excel("data-raw/binge_params/Hill-McManus_binge_params.xlsx", sheet = "Table 6") %>% setDT

# Put in a list
binge_params <- list(binge_params_table3, binge_params_table5, binge_params_table6)

# Save the result to the package data folder
usethis::use_data(binge_params, overwrite = T)






