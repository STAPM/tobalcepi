
# This code reads the processed HSE datasets for each year
# and conducts imputation.

# all the variables to be imputed are categorical - to feed into Multiple Correspondence Analysis

library(data.table)
library(hseclean)


# choose the file output by 10_HSE_variable_processing.R

data <- readRDS("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/England/tobalc_consumption_eng_national_2011-2018_v1_2023-10-26_hseclean_1.11.3.rds")


# view variables with missingness
misscheck <- function(var) {
  x <- table(var, useNA = "ifany")
  na <- x[which(is.na(names(x)))]
  if(length(na) == 0) na <- 0
  perc <- round(100 * na / sum(x), 2)
  #return(c(paste0(na, " missing obs, ", perc, "%")))
  return(na)
}

n_missing <- sapply(data, misscheck)
missing_vars <- n_missing[which(n_missing > 0)]
missing_vars

# household equivalised income has the most missingness 
# - this is a key variable to impute
# as we will use it to understand policy inequalities

# Set order of factors where needed for imputing as ordered.
data[ , kids := factor(kids, levels = c("0", "1", "2", "3+"))]
data[ , income5cat := factor(income5cat, levels = c("1_lowest_income", "2", "3", "4", "5_highest_income"))]

# Impute missing values

# Run the imputation
imp <- impute_data_mice(
  data = data,
  var_names = c(
    "sex",
    "age_cat",
    "kids",
    "relationship_status",
    "ethnicity_4cat",
    "ethnicity_2cat",
    "imd_quintile",
    "eduend4cat",
    "degree",
    "nssec3_lab",
    "employ2cat",
    "activity_lstweek",
    "income5cat",
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
    "cig_smoker_status",
    "giveup_smk",
    "smoker_cat",
    "banded_consumption",
    "cig_type",
    "time_to_first_cig",
    "drinks_now",
    "drinker_cat",
    "spirits_pref_cat",
    "wine_pref_cat",
    "rtd_pref_cat",
    "beer_pref_cat",
    "binge_cat",
    "bmi_4cat",
    "social_grade"
  ),
  var_methods = c(
    "",
    "",
    "polr",
    "polyreg",
    "polyreg",
    "polyreg",
    "",
    "polyreg",
    "logreg",
    "polyreg",
    "",
    "",
    "polr",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "logreg",
    "",
    "polyreg",
    "",
    "",
    "polyreg",
    "polyreg",
    "",
    "",
    "",
    "",
    "",
    "",
    "polyreg",
    "",
    "polyreg"
  ),
  n_imputations = 5
  # for testing just do 1 imputation
  # but test with more later
  # for point estimates, apparently 2-10 imputations are enough
)

data_imp <- copy(imp$data)

#write.table(data_imp, "data/HSE_2011_to_2017_imputed.csv", row.names = FALSE, sep = ",")

# note the package version so that the data can be tagged with it
ver <- packageVersion("hseclean")

saveRDS(data_imp, paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/England/tobalc_consumption_eng_national_2011-2018_v1_", Sys.Date(), "_hseclean_", ver, "_imputed.rds"))



