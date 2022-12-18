
# The aim of this code is to clean the tobacco and alcohol data from the Scottish Health Survey

# note: no questions asked to < 16 year olds in Shes

# Using functions in the hseclean package
library(hseclean)
library(data.table)
library(magrittr)

# Location of Scottish data
root_dir <- "X:/HAR_PR/PR/Consumption_TA/HSE/Scottish Health Survey (SHeS)/"

# The variables to retain
keep_vars = c(
  "hse_id", "wt_int", "psu", "cluster", "year", "quarter",
  "age", "age_cat", "sex", "imd_quintile",
  "ethnicity_2cat",
  "degree", "marstat", "relationship_status", "employ2cat", "social_grade", "kids", "income5cat",
  "nssec3_lab", "man_nonman", "activity_lstweek", "eduend4cat",

  "hse_mental",
  
  "weight", "height",

  "drinks_now",
  "drink_freq_7d", "n_days_drink", "peakday", "binge_cat",
  "beer_units", "wine_units", "spirit_units", "rtd_units",
  "weekmean", "drating", "dnoft", "dnnow", "dnany", "dnevr",
  "perc_spirit_units", "perc_wine_units", "perc_rtd_units", "perc_beer_units",
  "drinker_cat",
  #"spirits_pref_cat", "wine_pref_cat", "rtd_pref_cat", "beer_pref_cat",
  "total_units7_ch",
  
  # Smoking
  "cig_smoker_status",
  "years_since_quit", "years_reg_smoker", "cig_ever",
  "smk_start_age", "smk_stop_age", "censor_age", 
  "cigs_per_day", "smoker_cat", "hand_rolled_per_day", "machine_rolled_per_day", "prop_handrolled", "cig_type")


# Main processing

cleandata <- function(data) {

  data <- clean_age(data)
  data <- clean_demographic(data)
  data <- clean_education(data)
  data <- clean_economic_status(data)
  data <- clean_family(data)
  data <- clean_income(data)
  data <- clean_health_and_bio(data)

  data <- alc_drink_now_allages(data)
  data <- alc_weekmean_adult(data)
  data <- alc_sevenday_adult(data)
  
  data <- smk_status(data)
  data <- smk_former(data)
  data <- smk_life_history(data)
  data <- smk_amount(data)
  
  data <- select_data(
    data,
    ages = 16:89,
    years = 2008:2019,
    keep_vars = keep_vars,
    complete_vars = c("age", "sex", "imd_quintile", "psu", "cluster", "year")
  )

  return(data)
}

shes_data <- combine_years(list(
  cleandata(read_SHeS_2008(root = root_dir)),
  cleandata(read_SHeS_2009(root = root_dir)),
  cleandata(read_SHeS_2010(root = root_dir)),
  cleandata(read_SHeS_2011(root = root_dir)),
  cleandata(read_SHeS_2012(root = root_dir)),
  cleandata(read_SHeS_2013(root = root_dir)),
  cleandata(read_SHeS_2014(root = root_dir)),
  cleandata(read_SHeS_2015(root = root_dir)),
  cleandata(read_SHeS_2016(root = root_dir)),
  cleandata(read_SHeS_2017(root = root_dir)),
  cleandata(read_SHeS_2018(root = root_dir)),
  cleandata(read_SHeS_2019(root = root_dir))
))

# Load population data for Scotland
# from here - X:\ScHARR\PR_Mortality_data_TA\data\Processed pop sizes and death rates from VM

scot_pops <- fread("X:/ScHARR/PR_Mortality_data_TA/data/Processed pop sizes and death rates from VM/pop_sizes_scotland_national_v1_2022-12-13_mort.tools_1.5.0.csv")
setnames(scot_pops, c("pops"), c("N"))

# adjust the survey weights according to the ratio of the real population to the sampled population
shes_data <- clean_surveyweights(shes_data, pop_data = scot_pops)

# remake age categories
shes_data[, age_cat := c("16-17",
                         "18-24",
                         "25-34",
                         "35-44",
                         "45-54",
                         "55-64",
                         "65-74",
                         "75-89")[findInterval(age, c(-1, 18, 25, 35, 45, 55, 65, 75, 1000))]]

setnames(shes_data,
         c("smk_start_age", "cig_smoker_status", "years_since_quit"),
         c("start_age", "smk.state", "time_since_quit"))

# Checks on data

shes_data[(spirit_units + wine_units + rtd_units + beer_units) != weekmean]

shes_data[drinker_cat != "abstainer" & weekmean == 0]

# some drinkers have no data for average weekly consumption
# remove these individuals from the dataset - rather than imputing the missing data
shes_data <- shes_data[!(drinker_cat != "abstainer" & weekmean == 0)]

shes_data[drinker_cat != "abstainer" & is.na(weekmean)]

# select only rows with complete information on average weekly alcohol consumption
shes_data <- shes_data[!is.na(weekmean)]

shes_data[smoker_cat != "non_smoker" & cigs_per_day == 0]

# select only rows with complete information on average weekly alcohol consumption
shes_data <- shes_data[!is.na(smk.state)]

nrow(shes_data)

#shes_data[is.na(drinker_cat)]

######## Write the data

# note the package version so that the data can be tagged with it
ver <- packageVersion("hseclean")

saveRDS(shes_data, paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/Scotland/tobalc_consumption_scot_national_2008-2019_v1_", Sys.Date(), "_hseclean_", ver, ".rds"))

