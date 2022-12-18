
# The aim of this code is to calculate weighted averages of the 
# parameters from Hill-McManus et al 2014
# to produce parameters for use in STAPM 
# stratified by age, sex and IMD quintile

# Base this on the Health Survey for England 2011-2017
# Detailed recording of alcohol data in the HSE began in 2011
# the latest available year at the time of writing this code is 2017

# do not save the intermediate datasets as don't want these uploaded to gitlab
# (but for development purposes the intermediate datasets were saved locally)

library(data.table)
library(tobalcepi)

set.seed(1)

# Requires connection to the X drive to read the HSE data
# takes a while to run
source("data-raw/binge_params/10 HSE variable processing.R", echo = F, verbose = F)
source("data-raw/binge_params/12 Imputation.R", echo = F, verbose = F)

# Read the imputed HSE data sample
#data <- fread("data-raw/binge_params/HSE_2011_to_2017_imputed.csv")

# Test the existing AlcBinge function on these data
# This is the SAPM method
#data <- tobalcepi::AlcBinge(data)

# Replicate some of the code from the AlcBinge functions
# with the aim of assigning the relevant parameters to the data before averaging them

# add temporary age category
data[, age_temp := c(
  "<16", "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
  "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")[findInterval(age, c(-1, 16, 18, seq(20, 90, 5)))]]

# coefficients based on 2014 Hill-McManus et al

# negative binomial regression model for the number of weekly drinking occasions - Table 3
freq_model_coef <- binge_params[[1]]$coefficient

# fitted Heckman selection model for probability that
# an individual drinks on at least 3 separate occasions during the diary period - Table 5
select_model_coef <- binge_params[[2]]$coefficient

# fitted Heckman outcome regression results for the standard deviation
# in the quantity of alcohol consumed in a drinking occasion - Table 6
sdv_model_coef <- binge_params[[3]]$coefficient


################################
################################

# calculate expected number of weekly drinking occasions, using freq_model_coef
# This just creates a new column for each variable,
# and allocates the individual a coefficient based on their characteristics.

data[ , mean_consump_coef := freq_model_coef[1]]

data[ , age_coef := 0]
data[age_temp %in% c("25-29", "30-34"), age_coef := freq_model_coef[2]]
data[age_temp %in% c("35-39", "40-44"), age_coef := freq_model_coef[3]]
data[age_temp %in% c("45-49", "50-54"), age_coef := freq_model_coef[4]]

# model applied to population below 65 years, but assume effect at 55-65 applies at older ages too
data[age_temp %in% c("55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
     age_coef := freq_model_coef[5]]

data[ , income_coef := 0]
data[income5cat == "1_lowest_income", income_coef := freq_model_coef[6]]

data[ , ethn_coef := 0]
data[ethnicity_2cat == "non_white", ethn_coef := freq_model_coef[7]]

data[ , leaveed_coef := 0]
data[eduend4cat == "never_went_to_school", leaveed_coef := freq_model_coef[8]]
data[eduend4cat == "15_or_under", leaveed_coef := freq_model_coef[9]]
data[eduend4cat == "16-18", leaveed_coef := freq_model_coef[10]]

data[ , child_coef := 0]
data[kids == "1", child_coef := freq_model_coef[11]]
data[kids == "2", child_coef := freq_model_coef[12]]
data[kids == "3+", child_coef := freq_model_coef[13]]

data[ , class_coef := 0]
data[social_grade == "C2DE", class_coef := freq_model_coef[14]]

data[ , const_coef := freq_model_coef[15]]

# Calculate the weighted average of parameters by age, sex and IMD quintile
freq_model_coef_av <- data[ , .(
  mean_consump_coef = sum(mean_consump_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  age_coef = sum(age_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  imd_coef = sum((income_coef + ethn_coef + leaveed_coef + child_coef + class_coef) * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  const_coef = sum(const_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T) 
), by = c("age_cat", "sex", "imd_quintile")]

data[ , `:=`(mean_consump_coef = NULL, age_coef = NULL, income_coef = NULL, ethn_coef = NULL,
             leaveed_coef = NULL, child_coef = NULL, class_coef = NULL, const_coef = NULL)]

################################
################################

# expected standard deviation of drinking occasions

data[ , mean_consump_coef := select_model_coef[1]]

data[ , age_coef := 0]
data[age_temp %in% c("25-29", "30-34"), age_coef := select_model_coef[2]]
data[age_temp %in% c("35-39", "40-44"), age_coef := select_model_coef[3]]
data[age_temp %in% c("45-49", "50-54"), age_coef := select_model_coef[4]]
data[age_temp %in% c("55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
     age_coef := select_model_coef[5]]

data[ , employ_coef := 0]
data[employ2cat == "unemployed", employ_coef := select_model_coef[6]]

data[ , income_coef := 0]
data[income5cat == "1_lowest_income", income_coef := select_model_coef[7]]

data[ , ethn_coef := 0]
data[ethnicity_2cat == "non_white", ethn_coef := select_model_coef[8]]

data[ , leaveed_coef := 0]
data[eduend4cat == "never_went_to_school", leaveed_coef := select_model_coef[9]]
data[eduend4cat == "15_or_under", leaveed_coef := select_model_coef[10]]
data[eduend4cat == "16-18", leaveed_coef := select_model_coef[11]]

data[ , child_coef := 0]
data[kids == "1", child_coef := select_model_coef[12]]
data[kids == "2", child_coef := select_model_coef[13]]
data[kids == "3+", child_coef := select_model_coef[14]]

data[ , class_coef := 0]
data[social_grade == "C2DE", class_coef := select_model_coef[15]]

data[ , const_coef := select_model_coef[16]]

# Calculate the weighted average of parameters by age, sex and IMD quintile
select_model_coef_av <- data[ , .(
  mean_consump_coef = sum(mean_consump_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  age_coef = sum(age_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  imd_coef = sum((employ_coef + income_coef + ethn_coef + leaveed_coef + child_coef + class_coef) * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  const_coef = sum(const_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T) 
), by = c("age_cat", "sex", "imd_quintile")]

data[ , `:=`(mean_consump_coef = NULL, age_coef = NULL, employ_coef = NULL, income_coef = NULL,
             ethn_coef = NULL, leaveed_coef = NULL, child_coef = NULL, class_coef = NULL, const_coef = NULL)]

################################
################################

# occasion level standard deviation of the quantity consumed in a drinking occasion

data[ , mean_consump_coef := sdv_model_coef[1]]

data[ , income_coef := 0]
data[income5cat == "1_lowest_income", income_coef := sdv_model_coef[2]]

data[ , imr_coef := sdv_model_coef[3]]

# Calculate the weighted average of parameters by age, sex and IMD quintile
sdv_model_coef_av <- data[ , .(
  mean_consump_coef = sum(mean_consump_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  imd_coef = sum(income_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  imr_coef = sum(imr_coef * wt_int, na.rm = T) / sum(wt_int, na.rm = T) 
), by = c("age_cat", "sex", "imd_quintile")]

data[ , `:=`(mean_consump_coef = NULL, income_coef = NULL, imr_coef = NULL)]

################################
################################

# average of height and weight
height_weight_av <- data[ , .(
  height = sum(height * wt_int, na.rm = T) / sum(wt_int, na.rm = T),
  weight = sum(weight * wt_int, na.rm = T) / sum(wt_int, na.rm = T)
), by = c("age_cat", "sex", "imd_quintile")]

################################
################################

# put the data into a list
binge_params_stapm <- list(freq_model_coef_av, select_model_coef_av, sdv_model_coef_av, height_weight_av)

# Save the result to the package data folder
usethis::use_data(binge_params_stapm, overwrite = T)


