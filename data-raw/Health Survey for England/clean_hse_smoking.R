

#install.packages("X:/ScHARR/PR_STAPM/Code/R_packages/hseclean_0.1.0.zip", repos = NULL)

library(hseclean)

cleandata <- function(data) {

  data <- clean_age(data)
  data <- clean_family(data)
  data <- clean_demographic(data)
  data <- clean_education(data)
  data <- clean_economic_status(data)
  data <- clean_income(data)
  data <- clean_health_and_bio(data)

  data <- smk_status(data)
  data <- smk_former(data)
  data <- smk_life_history(data)
  data <- smk_amount(data)

  data <- select_data(
    data,
    ages = 12:89,
    years = 2001:2016,
    keep_vars = c("age", "sex", "imd_quintile", "wt_int", "psu", "cluster", "year", "age_cat", "cig_smoker_status",
                  "smk_start_age", "censor_age", "cigs_per_day", "smoker_cat",
                  "years_since_quit", "degree", "relationship_status", "employ2cat",  "hse_mental", "hse_heart", "hse_respir", "hse_endocrine", "kids", "income5cat"),
    complete_vars = c("age", "sex", "imd_quintile", "cig_smoker_status", "psu", "wt_int", "cluster", "year", "censor_age")
  )

return(data)
}

hse_data <- combine_years(list(
  cleandata(read_2001()),
  cleandata(read_2002()),
  cleandata(read_2003()),
  cleandata(read_2004()),
  cleandata(read_2005()),
  cleandata(read_2006()),
  cleandata(read_2007()),
  cleandata(read_2008()),
  cleandata(read_2009()),
  cleandata(read_2010()),
  cleandata(read_2011()),
  cleandata(read_2012()),
  cleandata(read_2013()),
  cleandata(read_2014()),
  cleandata(read_2015()),
  cleandata(read_2016())
))

hse_data <- clean_surveyweights(hse_data)

setnames(hse_data,
         c("smk_start_age", "cig_smoker_status", "years_since_quit"),
         c("start_age", "smk.state", "time_since_quit"))

hse_data[is.na(degree), degree := "no_degree"]
hse_data[is.na(relationship_status ), relationship_status  := "single"]
hse_data[is.na(employ2cat), employ2cat  := "unemployed"]
hse_data[is.na(hse_mental), hse_mental  := "no_mental"]
hse_data[is.na(hse_heart), hse_heart  := "no_heart"]
hse_data[is.na(hse_respir), hse_respir  := "no_respir"]
hse_data[is.na(hse_endocrine), hse_endocrine  := "no_endocrine"]
hse_data[is.na(kids), kids  := "0"]
hse_data[is.na(income5cat), income5cat  := "1_lowest_income"]

hse_data[ , time_since_quit := as.double(ceiling(time_since_quit))]
hse_data <- hse_data[!(smk.state == "former" & time_since_quit < 1)]


# Main data
hse_data_smoking <- copy(hse_data)

testthat::expect_equal(nrow(hse_data_smoking[smk.state == "current" & cigs_per_day == 0]), 0,
                       info = "some current smokers smoke 0 cigs per day")

# Save the data to the package data folder
usethis::use_data(hse_data_smoking, overwrite = TRUE)

rm(hse_data_smoking, hse_data, cleandata)
gc()






