
#' Calculate variables to inform alcohol binge model - STAPM version
#' 
#' \lifecycle{maturing}
#'
#' Designed to work with simulated individual trajectories of alcohol consumption - stratified by 
#' age category, sex and IMD quintile. Assigns coefficients stratified by age category, sex and IMD quintile to 
#' the simulated sample of individuals to estimate their characteristics of single occassion drinking 
#' at each time step in the simulation.  
#'
#' The coefficients used come originally from a study by Hill-McManus 2014,
#' who analysed drinking occasions using data from detailed diaries in the National Diet and Nutrition Survey 2000/2001.
#' Using the results, it possible to model each individual's expected number of drinking occasions across the year,
#' the average amount they drunk on an occasion, the variability in the amount drunk among occasions,
#' and how these vary socio-demographically.   
#' 
#' To get these coefficients into a form in which they fit with the age, sex and IMD quintile 
#' stratification of the STAPM model required assigning them to a sample of Health Survey for England data (2011-2017) 
#' based on a full range of covariates, and then calculating weighted averages by age category, sex and IMD quintile.  
#' 
#' This function is designed to be applied at each time step during a STAPM model run. 
#'
#' @param data Data table of individual characteristics - the variables used are average weekly alcohol consumption, 
#' age, sex and IMD quintile.
#' @param params List of four data tables - three containing coefficient estimates from Hill-McManus et al 
#' averaged by age category, sex and IMD quintile, and the fourth containing estimates of individual height and weight 
#' averaged by age category, sex and IMD quintile.    
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
#'   age = rpois(n, 30),
#'   sex = sample(x = c("Male", "Female"), size = n, replace = T),
#'   imd_quintile = "5_most_deprived"
#' )
#'
#' test_data <- AlcBinge_stapm(data)
#'}
#'
AlcBinge_stapm <- function(
  data,
  params = tobalcepi::binge_params_stapm
) {
  
  # Create age category variable - 
  # to match the age categories used to summarise the coefficients from Hill-McManus et al
  data[ , age_cat := 
          c("11-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89")[
            findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75))]]
  
  
  # Calculate the expected number of weekly drinking occasions
  
  data <- merge(data, params[[1]],
                by = c("age_cat", "sex", "imd_quintile"),
                all.x = T, all.y = F, sort = F)
  
  data[ , drink_freq := exp(mean_consump_coef * log(weekmean) + age_coef + imd_coef + const_coef)]
  
  data[ , `:=`(mean_consump_coef = NULL, age_coef = NULL, imd_coef = NULL, const_coef = NULL)]
  
  data[weekmean == 0, drink_freq := 0]
  
  ################
  
  # Calculate expected standard deviation of a drinking occasions
  
  # Step one: calculate probability of having 3 or more drinking occasions in a week
  
  data <- merge(data, params[[2]],
                by = c("age_cat", "sex", "imd_quintile"),
                all.x = T, all.y = F, sort = F)
  
  data[ , drink_3_or_more := VGAM::probitlink(mean_consump_coef * log(weekmean) + age_coef + imd_coef + const_coef, inverse = T)]
  
  data[ , `:=`(mean_consump_coef = NULL, age_coef = NULL, imd_coef = NULL, const_coef = NULL)]
  
  data[weekmean == 0, drink_3_or_more := 0]
  
  
  # Step 2: calculate inverse mills ratio
  
  # Formula taken from Hill-McManus 2014
  # standard normal density function / (1 - standard normal cumulative distribution function)
  
  data[ , imr := stats::dnorm(drink_3_or_more) / (1 - stats::pnorm(drink_3_or_more))]
  
  
  # Step 3 : calculate the predicted occasion level standard deviation
  # (variation in the quantity consumed in a drinking occasion)
  
  data <- merge(data, params[[3]],
                by = c("age_cat", "sex", "imd_quintile"),
                all.x = T, all.y = F, sort = F)
  
  data[ , occ_sd := exp(mean_consump_coef * log(weekmean) + imd_coef + imr_coef * imr) / 8]
  
  # The paper appears to say it is linear regression, but after confirming with Dan, the y (i.e., standard deviation)
  # is acutally logged. The paper also not clear regarding measurements.
  # But it turned to be units for all weekly consumption (independent variable) and gram for standard deviation of the model.
  # hence divided by 8.
  
  data[ , `:=`(mean_consump_coef = NULL, imd_coef = NULL, imr_coef = NULL, imr = NULL, drink_3_or_more = NULL)]
  
  
  # calculate the average quantity of alcohol consumed during a drinking occasion,
  # obtained using the mean weekly consumption divided
  # by the predicted number of weekly drinking occasions.
  
  data[ , mean_sod := weekmean / drink_freq]
  data[weekmean == 0, mean_sod := 0]
  
  
  # Calculate the Wildemark r value for each individual using their weight and height from the HSE
  # described in Watson 1981
  
  data <- merge(data, params[[4]],
                by = c("age_cat", "sex", "imd_quintile"),
                all.x = T, all.y = F, sort = F)
  
  data[sex == "Male", rwatson := 0.39834 + ((12.725 * height - 0.11275 * age + 2.8993) / weight)]
  data[sex == "Female", rwatson := 0.29218 + ((12.666 * height - 2.4846) / weight)]
  
  data[ , `:=`(age_cat = NULL, height = NULL)]
  
  
  return(data[])
}



