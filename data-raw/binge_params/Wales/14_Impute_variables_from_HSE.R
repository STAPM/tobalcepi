### read in HSE imputed dataset for the English binge model parameters and 
### combine with the NSW data. Use English data to impute edu4end and social_grade
### variables in the NSW

data_imp_hse <- readRDS(paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/England/tobalc_consumption_eng_national_2011-2018_v1_2023-10-26_hseclean_1.11.3_imputed.rds"))
data_imp_hse[, country := "England"]

data_imp_nsw <- readRDS(paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/Wales/tobalc_consumption_wales_2016-2022_v1_", Sys.Date(), "_hseclean_1.14.0_imputed.rds"))
data_imp_nsw[, country := "Wales"]

data <- rbindlist(list(data_imp_hse, data_imp_nsw), fill = TRUE)

### Imputation

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
    #"nssec3_lab",
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
    #"hse_other",
    "cig_smoker_status",
    #"giveup_smk",
    #"smoker_cat",
    #"banded_consumption",
    #"cig_type",
    #"time_to_first_cig",
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
    #"polyreg",
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
    #"logreg",
    "",
    #"polyreg",
    #"",
    #"",
    #"polyreg",
    #"polyreg",
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
  n_imputations = 1
)

data_imp <- copy(imp$data)
data_imp <- data_imp[country == "Wales"]
data_imp[,"country" := NULL]

saveRDS(data_imp, paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/Wales/tobalc_consumption_wales_2016-2022_v1_FINAL_imputed.rds"))

