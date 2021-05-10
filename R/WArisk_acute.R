
#' Risk of acute conditions wholly-attributable to alcohol \lifecycle{stable}
#'
#' Uses the 'new' binge model methods to calculate the risk
#' that each individual experiences each acute consequence of drinking during one year 
#' (e.g. alcohol poisoning or the effects of acute intoxication). 
#'
#' The function implements a new method for estimating risk that was developed to suit the STAPM modelling. 
#' The calculation is based on the link between average weekly alcohol consumption and 
#' the distribution of characteristics of single occasion drinking described by the 
#' parameter estimates of Hill-McManus et al 2014.     
#' 
#' The function uses the outputs of AlcBinge_stapm() 
#' to estimate for each individual: (1) the average amount that each individual is expected to
#' drink on a single drinking occasion; (2) the standard deviation of the amount that each individual is expected to
#' drink on a single drinking occasion; (3) the expected number of drinking occasions that
#' each individual has each week.    
#' 
#' Based on those estimates, a probability distribution is generated over the 
#' number of units of alcohol that could be consumed on a single drinking occasion by each individual. 
#' Values for the number of units that fall below the binge drinking thresholds (6 units a day for women, 
#' 8 units a day for men) are set to zero. The probability distribution is then used to compute the 
#' total number of units above the binge thresholds that each individual is expected to drink in a year. 
#' We assume that each individual's risk is proportional to that value.  
#' 
#'
#' @param SODMean Numeric - the average amount that each individual is expected to
#' drink on a single drinking occasion.
#' @param SODSDV Numeric - the standard deviation of the amount that each individual is expected to
#' drink on a single drinking occasion.
#' @param SODFreq Numeric - the expected number of drinking occasions that
#' each individual has each week.
#' @param sex Character - individual sex (Male or Female).
#' @param grams_ethanol_per_unit Numeric value giving the conversion factor for the number of grams of pure
#' ethanol in one UK standard unit of alcohol.
#' @param alc_wholly_acute_thresholds Numeric vector - the thresholds in UK standard units of alcohol /day over
#'  which individuals begin to experience an elevated risk
#'  for acute diseases that are wholly attributable to alcohol. Input in the form c(female, male). 
#'  Defaults to 3 units/day for females and 4 units/day for males.   
#'
#' @return Returns a numeric vector of each individual's relative risk of the acute consequences of drinking.
#' @importFrom data.table := setDT setnames
#' @export
#' 
#'
#' @examples
#' 
#' \dontrun{
#' 
#' # Function called within RRAlc()
#' 
#' data[ , ar := sapply(1:n, function(z) {
#' tobalcepi::WArisk_acute(
#'   SODMean = mean_sod[z],
#'   SODSDV = occ_sd[z],
#'   SODFreq = drink_freq[z],
#'   sex = sex[z],
#'   grams_ethanol_per_unit = grams_ethanol_per_unit,
#'   alc_wholly_acute_thresholds = alc_wholly_acute_thresholds
#' )
#' })]
#' 
#' risk_indiv <- 1 + data[ , ar] # add 1 to remove 0/0 = Not a number error later
#' 
#' data[ , ar := NULL]
#' 
#' }
#' 
WArisk_acute <- function(
  SODMean,
  SODSDV,
  SODFreq,
  sex,
  grams_ethanol_per_unit = 8,
  alc_wholly_acute_thresholds = c(3, 4)
) {
  
  # SODMean <- 18
  # SODSDV <- 14
  # SODFreq <- 4
  # sex <- "Male"
  # grams_ethanol_per_unit <- 8
  # alc_wholly_acute_thresholds <- c(6, 8)
  
  
  
  # The amounts of alcohol (g ethanol) that could be consumed on an occasion
  # i.e. the mass of alcohol ingested
  grams_ethanol <- 1:600 # units * ConvertToGramOfAlcohol#1:100
  
  # Calculate the cumulative probability distribution of each amount of alcohol (1 to 100 g) 
  # being drunk on an occasion
  x <- stats::pnorm(
    grams_ethanol,
    SODMean * grams_ethanol_per_unit, # mean
    SODSDV * grams_ethanol_per_unit # variance
  )
  
  # Convert from the cumulative distribution to the
  # probability that each level of alcohol is consumed on a drinking occasion
  #interval_prob <- x - c(0, x[1:(length(x) - 1)])
  interval_prob <- diff(x)
  
  interval_prob <- interval_prob / sum(interval_prob)
  
  # Units consumed above the binge threshold
  
  if(sex == "Female") {
    threshold <- alc_wholly_acute_thresholds[1] # 6 units
  }
  
  if(sex == "Male") {
    threshold <- alc_wholly_acute_thresholds[2] # 8 units
  }
  
  # Convert grams of ethanol back to units
  units_vec <- grams_ethanol[1:(600 - 1)] / grams_ethanol_per_unit
  
  # Subtract the threshold and replace negative values with zero
  units_vec <- units_vec - threshold
  units_vec <- replace(units_vec, units_vec < 0, 0)
  
  
  # Calculate the total number of units drunk above the binge threshold
  
  units_above_threshold <-
    SODFreq * # expected number of weekly drinking occasions [number]
    52 * # multiply by the number of weeks in a year [number]
    interval_prob * # the probability that each level of alcohol is consumed on a drinking occasion [vector]
    units_vec # units that are above the threshold
  
  # Total annual units drunk above the binge threshold
  units_above_threshold_sum <- sum(units_above_threshold)
  
  
  
  # rm(
  #   grams_ethanol, 
  #   x,
  #   interval_prob,
  #   threshold,
  #   units_vec,
  #   units_above_threshold
  # )
  # gc()
  
  
  
  return(units_above_threshold_sum)
}





