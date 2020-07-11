
# Processing tobacco and alcohol data from the Health Survey for England

# install and load the hseclean package (with version specified)
#devtools::install_github("STAPM/hseclean", ref = "1.0.0")

library(hseclean)
library(magrittr)
library(data.table)

root_dir <- "/Volumes/Shared/"

# apply functions to create the variables for analysis and to retain only the required variables

# The variables to retain
keep_vars = c(
  # Survey design variables
  "wt_int",
  "psu",
  "cluster",
  "year",
  
  # Social / economic / demographic variables
  "age",
  "age_cat",
  "sex",
  "imd_quintile",
  "ethnicity_4cat",
  "ethnicity_2cat",
  "degree",
  "relationship_status",
  "employ2cat",
  "kids",
  "income5cat",
  "nssec3_lab",
  "activity_lstweek",
  "eduend4cat",
  "social_grade",
  
  # Long term health conditions
  "hse_cancer",
  "hse_endocrine",
  "hse_heart",
  "hse_mental",
  "hse_nervous",
  "hse_eye",
  "hse_ear",
  "hse_respir",
  "hse_disgest",
  "hse_urinary",
  "hse_skin",
  "hse_muscskel",
  "hse_infect",
  "hse_blood",
  "hse_other",
  
  "bmi", "weight", "height",
  
  # Smoking
  "cig_smoker_status",
  "smoker_cat",
  "cig_type",
  "time_to_first_cig",
  "giveup_smk",
  "banded_consumption",
  
  # Drinking
  "drinks_now",
  "drinker_cat",
  "spirits_pref_cat",
  "wine_pref_cat",
  "rtd_pref_cat",
  "beer_pref_cat",
  "binge_cat",
  "weekmean"
)

# The variables that must have complete cases
complete_vars <- c("age", "sex", "year", "psu", "cluster", "cig_smoker_status", "drinker_cat")


#-----------------------------------------------------
# Read and clean the HSE tobacco and alcohol data

cleandata <- function(data) {
  
  data %<>%
    clean_age %>%
    clean_demographic %>% 
    clean_education %>%
    clean_economic_status %>%
    clean_family %>%
    clean_income %>%
    clean_health_and_bio %>%
    smk_status %>%
    smk_former %>%
    smk_quit %>%
    smk_life_history %>%
    smk_amount %>%
    alc_drink_now_allages %>%
    alc_weekmean_adult %>%
    alc_sevenday_adult %>%
    alc_sevenday_child %>%
    
    select_data(
      ages = 11:89,
      years = 2011:2017,
      
      # variables to retain
      keep_vars = keep_vars,
      
      # The variables that must have complete cases
      complete_vars = complete_vars
    )
  
  return(data)
}

# Read and clean each year of data and bind them together in one big dataset
data <- combine_years(list(
  cleandata(read_2011(root = root_dir)),
  cleandata(read_2012(root = root_dir)),
  cleandata(read_2013(root = root_dir)),
  cleandata(read_2014(root = root_dir)),
  cleandata(read_2015(root = root_dir)),
  cleandata(read_2016(root = root_dir)),
  cleandata(read_2017(root = root_dir))
))

# clean the survey weights
data <- clean_surveyweights(data)

# remake age categories
data[, age_cat := c("11-15",
                    "16-17",
                    "18-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-64",
                    "65-74",
                    "75-89")[findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]


#-----------------------------------------------------
# Categorise BMI

# Adult BMI categories
data[age >= 20, bmi_4cat := c(
  "underweight",
  "normal",
  "overweight",
  "obese"
)[findInterval(bmi, c(-1000, 18.5, 25, 30))]]

# Child and teen BMI categories
# based on percentiles

# separate kid and adult data
data_kids <- data[age < 20]
data_adults <- data[age >= 20]

# sort kid data ascending by bmi
setorderv(data_kids, "bmi", 1)

# calculate the cumulative sum of the survey weights by age and sex

# make a temporary age category
data_kids[, age_cat_temp := c("11-12",
                    "13-14",
                    "15-16",
                    "17-19")[findInterval(age, c(-1, 13, 15, 17, 1000))]]

# Calculate percentiles
data_kids[ , cum_svy_wgt := cumsum(wt_int), by = c("sex", "age_cat_temp")]
data_kids[ , cum_svy_wgt := cum_svy_wgt / max(cum_svy_wgt), by = c("sex", "age_cat_temp")]

# assign categories
data_kids[ , bmi_4cat := c(
  "underweight",
  "normal",
  "overweight",
  "obese"
)[findInterval(cum_svy_wgt, c(-1000, .05, .85, .95))]]

data_kids[ , `:=`(age_cat_temp = NULL, cum_svy_wgt = NULL)]

data <- data.table::rbindlist(list(data_kids, data_adults), use.names = TRUE)

data[ , bmi := NULL]

rm(data_kids, data_adults)


#data <- copy(data[age >= 18])

#write.table(data, "data/HSE_2011_to_2017.csv", row.names = FALSE, sep = ",")











