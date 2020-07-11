
# The aim of this code is to develop and test the method to 
# model acute harms that are partially attributable to alcohol in STAPM

library(data.table)
library(tobalcepi)

# Read the imputed HSE data sample
data <- fread("data/HSE_2011_to_2017_imputed.csv")

setnames(data, c("ethnicity_2cat", "height", "weight"), c("ethnic2cat", "htval", "wtval"))

# Test the existing AlcBinge function on these data
# This is the SAPM method
data_sapm_test <- tobalcepi::AlcBinge(data)


# adapt the method to predict the number of drinking occassions 
# and the distribution of amount consumed 
# as a function of the average weekly amount of alcohol consumed, age, sex and IMD quintile. 

# assign the parameter values from Hill-McManus et al. to each individual 
# and then average these values by age, sex and IMD quintile 

# the IMD quintile averages will then reflect the variation among IMD quintiles 
# in the distribution of ethnicity, income, number of children etc.

# a copy of the AlcBinge function in tobalcepi has been moved to src for development
# function renamed AlcBinge_dev

data_sapm_test <- AlcBinge_dev(data)










