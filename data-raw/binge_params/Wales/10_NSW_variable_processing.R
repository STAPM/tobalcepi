# Processing tobacco and alcohol data from the National Survey for Wales

# use a sample with as many years as possible from the data
# Noting that:
# - the first year of detailed alcohol data was 2011
# - the first year of smoking product preference data was 2013

# include the variables commonly used to describe health and social inequalities
# that are also available in the HSE data

# use variables that describe smoking and drinking status and product preferences

library(hseclean)
library(magrittr)
library(data.table)


# apply functions to create the variables for analysis and to retain only the required variables

# The variables to retain
keep_vars = c(
  # "hse_id",
  "wt_int", "psu", "cluster", 
  "year", #"quarter",
  "age", "age_cat", "sex", "imd_quintile",
  "ethnicity_4cat", "ethnicity_2cat", "degree", 
  # "marstat", 
  "relationship_status", "employ2cat", "income5cat",  "kids",
  # "social_grade", "kids","nssec3_lab", "man_nonman", 
  "activity_lstweek", 
  # "eduend4cat",
  
  "hse_cancer", "hse_endocrine", "hse_heart", "hse_mental", "hse_nervous", "hse_eye", "hse_ear", "hse_respir",
  "hse_disgest", "hse_urinary", "hse_skin", "hse_muscskel", "hse_infect", "hse_blood",
  
  "weight", "height", "bmi",
  
  "cig_smoker_status",
  #  "years_since_quit", "years_reg_smoker", "cig_ever",
  #  "cigs_per_day", "smoker_cat", "banded_consumption", "time_to_first_cig",
  #  "smk_start_age", "smk_stop_age", "censor_age", "giveup_smk",
  #  "cig_type",
  #  "units_RYO_tob", "units_FM_cigs", "prop_handrolled",
  
  "drinks_now", "drink_freq_7d",
  "n_days_drink", "peakday", "binge_cat",
  "beer_units", "wine_units", "spirit_units", "rtd_units",
  "weekmean", 
  "perc_spirit_units", "perc_wine_units", "perc_rtd_units", "perc_beer_units",
  "drinker_cat",
  "spirits_pref_cat", "wine_pref_cat", "rtd_pref_cat", "beer_pref_cat"
  #  "totalwu", "total_units7_ch"
)

# The variables that must have complete cases
complete_vars <- c("age", "sex", "year","psu", "cluster","imd_quintile", 
                   #"censor_age", 
                   "cig_smoker_status")

complete_vars <- NULL

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
    #smk_life_history %>%       - none of these variables are in NSW - leave out and impute later
    #smk_amount %>%             - none of these variables are in NSW - leave out and impute later
    alc_drink_now_allages %>%
    alc_weekmean_adult %>%
    alc_sevenday_adult %>%
    #alc_sevenday_child %>%     - no children in NSW
    
    select_data(
      ages = 13:89,
      years = 2013:2022,
      
      # variables to retain
      keep_vars = keep_vars,
      
      # The variables that must have complete cases
      complete_vars = complete_vars
    )
  
  return(data)
}

# Read and clean each year of data and bind them together in one big dataset
data <- combine_years(list(
  cleandata(read_NSW_2016_17()),
  cleandata(read_NSW_2017_18()),
  cleandata(read_NSW_2018_19()),
  cleandata(read_NSW_2019_20()),
  cleandata(read_NSW_2020_21()),
  cleandata(read_NSW_2021_22()),
  cleandata(read_NSW_2022_23())
))



# Load population data for England
# from here - X:\ScHARR\PR_Mortality_data_TA\data\Processed pop sizes and death rates from VM

wales_pops <- stapmr::pop_counts_wales
#setnames(eng_pops, c("pops"), c("N"))

# adjust the survey weights according to the ratio of the real population to the sampled population
data <- clean_surveyweights(data, pop_data = wales_pops)


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

######## Write the data

# note the package version so that the data can be tagged with it
ver <- packageVersion("hseclean")

saveRDS(data, paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/Wales/tobalc_consumption_wales_2016-2022_v1_", Sys.Date(), "_hseclean_", ver, ".rds"))










