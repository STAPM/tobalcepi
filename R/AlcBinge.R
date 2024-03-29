
#' Calculate variables to inform alcohol binge model \lifecycle{superseded}
#'
#' Designed to work with cross-sectional survey data with a wide range of individual-level covariates. 
#' Uses survey data and previously estimated coefficients to describe
#' the patterns of single occasion drinking.
#'
#' This is based on a study by Hill-McManus 2014,
#' who analysed drinking occasions using data from detailed diaries in the National Diet and Nutrition Survey 2000/2001.
#' Using the results, it possible to model each individual's expected number of drinking occasions across the year,
#' the average amount they drunk on an occasion, the variability in the amount drunk among occasions,
#' and how these vary socio-demographically.
#'
#' @param data Data table of individual characteristics.
#' @param params List of three data tables containing the parameter estimates 
#' from Hill-McManus et al 2014, Tables 3, 5 and 6.  
#'
#' @return Returns data plus the estimated variables.
#' @importFrom data.table := setDT setnames
#' @export
#' 
#'
#' @examples
#'
#'\dontrun{
#'
#' # Simulate individual data
#'
#' # Using the parameters for the Gamma distribution from Kehoe et al. 2012
#' n <- 1e3
#' grams_ethanol_day <- rgamma(n, shape = 0.69, scale = 19.03)
#'
#' data <- data.table(
#'   weekmean = grams_ethanol_day * 7 / 8,
#'   peakday = grams_ethanol_day / 8,
#'   age = rpois(n, 30),
#'   sex = sample(x = c("Male", "Female"), size = n, replace = T),
#'   income5cat = "1_lowest income",
#'   imd_quintile = "5_most_deprived",
#'   kids = "0",
#'   social_grade = "C2DE",
#'   eduend4cat = "16-18", # age finished education
#'   ethnicity_2cat = "white", # white / non-white
#'   employ2cat = "yes", # employed / not
#'   weight = rnorm(n, mean = 60, sd = 5), # weight in kg
#'   height = rnorm(n, mean = 1.7, sd = .1) # height in m
#' )
#'
#' test_data <- AlcBinge(data)
#'}
#'
AlcBinge <- function(
  data,
  params = tobalcepi::binge_params
) {

  ##################################################################################
  # check variables
  temp <- nrow(data[is.na(age)])
  if(temp > 0) warning(paste0(temp, " missing values in age"), immediate. = T)

  temp <- nrow(data[is.na(income5cat)])
  if(temp > 0) warning(paste0(temp, " missing values in income5cat"), immediate. = T)

  temp <- nrow(data[is.na(kids)])
  if(temp > 0) warning(paste0(temp, " missing values in kids"), immediate. = T)

  temp <- nrow(data[is.na(social_grade)])
  if(temp > 0) warning(paste0(temp, " missing values in social_grade"), immediate. = T)

  temp <- nrow(data[is.na(eduend4cat)])
  if(temp > 0) warning(paste0(temp, " missing values in eduend4cat"), immediate. = T)

  temp <- nrow(data[is.na(ethnicity_2cat)])
  if(temp > 0) warning(paste0(temp, " missing values in ethnicity_2cat"), immediate. = T)

  temp <- nrow(data[is.na(employ2cat)])
  if(temp > 0) warning(paste0(temp, " missing values in employ2cat"), immediate. = T)

  ##################################################################################

  data[ , age_temp := c(
    "<16", "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")[
      findInterval(age, c(-1, 16, 18, seq(20, 90, 5)))]]

  ##################################################################################
  # coefficients based on 2014 Hill-McManus et al

  # negative binomial regression model for the number of weekly drinking occasions - Table 3
  freq_model_coef <- params[[1]]$coefficient

  # fitted Heckman selection model for probability that
  # an individual drinks on at least 3 separate occasions during the diary period - Table 5
  select_model_coef <- params[[2]]$coefficient
  
  # fitted Heckman outcome regression results for the standard deviation
  # in the quantity of alcohol consumed in a drinking occasion - Table 6
  sdv_model_coef <- params[[3]]$coefficient
  

  ##################################################################################

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

  # make the calculation

  data[ , drink_freq := exp(mean_consump_coef * log(weekmean) +
    age_coef + income_coef + ethn_coef + leaveed_coef + child_coef + class_coef + const_coef)]

  data[ , `:=`(mean_consump_coef = NULL, age_coef = NULL, income_coef = NULL, ethn_coef = NULL,
    leaveed_coef = NULL, child_coef = NULL, class_coef = NULL, const_coef = NULL)]

  data[weekmean == 0, drink_freq := 0]

  # calculate expected standard deviation of a drinking occasions, using sdv_model_coef

  # step one: calculate probability of having 3 or more drinking occasions in a week

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

  # make the calculation

  data[ , drink_3_or_more := VGAM::probitlink(mean_consump_coef * log(weekmean) +
    age_coef + employ_coef + income_coef + ethn_coef + leaveed_coef + child_coef + class_coef + const_coef, inverse = T)]

  data[ , `:=`(mean_consump_coef = NULL, age_coef = NULL, employ_coef = NULL, income_coef = NULL,
    ethn_coef = NULL, leaveed_coef = NULL, child_coef = NULL, class_coef = NULL, const_coef = NULL)]

  data[weekmean == 0, drink_3_or_more := 0]


  # step 2 : calculate inverse mills ratio

  # Formula taken from Hill-McManus 2014
  # standard normal density function / (1 - standard normal cumulative distribution function)

  data[ , imr := stats::dnorm(drink_3_or_more) / (1 - stats::pnorm(drink_3_or_more))]


  # step 3 : calculate the predicted occasion level standard deviation
  # (variation in the quantity consumed in a drinking occasion)

  data[ , mean_consump_coef := sdv_model_coef[1]]

  data[ , income_coef := 0]
  data[income5cat == "1_lowest_income", income_coef := sdv_model_coef[2]]

  data[ , imr_coef := sdv_model_coef[3]]

  data[ , occ_sd := exp(mean_consump_coef * log(weekmean) + income_coef + imr_coef * imr) / 8]

  # The paper appears to say it is linear regression, but after confirming with Dan, the y (i.e., standard deviation)
  # is acutally logged. The paper also not clear regarding measurements.
  # But it turned to be units for all weekly consumption (independent variable) and gram for standard deviation of the model.
  # hence divided by 8.

  data[ , `:=`(mean_consump_coef = NULL, income_coef = NULL, imr_coef = NULL)]


  # calculate the average quantity of alcohol consumed during a drinking occasion,
  # obtained using the mean weekly consumption divided
  # by the predicted number of weekly drinking occasions.

  data[ , mean_sod := weekmean / drink_freq]
  data[weekmean == 0, mean_sod := 0]


  # weights from the coefficients at the bottom of SAPM Binge code
  #Weight = ifelse(sex == 2, 63.42913136, 77.12631198)

  # Calculate the Wildemark r value for each individual using their weight and height from the HSE
  # described in Watson 1981

  # From SAPM binge code
  #data[sex == "Male", rwatson := 0.55]
  #data[sex == "Female", rwatson := 0.68]

  # Note that the numbers in the formulae below come from "The Estimation of Blood Alcohol Concentration - Widmark Revisited" - Posey and Mozayani
  # height must be in meters and weight in kg
  
  if(max(data$height, na.rm = T) > 100) {
    warning("AlcBinge: height in cm not m - correct the input data")
  }
  
  data[sex == "Male", rwatson := 0.39834 + ((12.725 * height - 0.11275 * age + 2.8993) / weight)]
  data[sex == "Female", rwatson := 0.29218 + ((12.666 * height - 2.4846) / weight)]

  
  data[ , age_temp := NULL]

return(data)
}



