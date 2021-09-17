
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
#' @param interval_prob_vec Column of vectors - the probabilities that each individual 
#' drinks each amount of grams of ethanol (1:600) on a single drinking occasion.
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
#' 
#' @importFrom data.table := setDT setnames fifelse
#' 
#' @export
#' 
#'
#' @examples
#' 
#' \dontrun{
#' 
#' # example needs updating
#' 
#' # Function called within RRAlc()
#' 
#' data[ , ar := 
#' tobalcepi::WArisk_acute(
#'   SODMean = mean_sod[z],
#'   SODSDV = occ_sd[z],
#'   SODFreq = drink_freq[z],
#'   sex = sex[z],
#'   grams_ethanol_per_unit = grams_ethanol_per_unit,
#'   alc_wholly_acute_thresholds = alc_wholly_acute_thresholds
#' )]
#' 
#' risk_indiv <- 1 + data[ , ar] # add 1 to remove 0/0 = Not a number error later
#' 
#' data[ , ar := NULL]
#' 
#' }
#' 
WArisk_acute <- function(
  interval_prob_vec,
  SODFreq,
  sex,
  grams_ethanol_per_unit = 8,
  alc_wholly_acute_thresholds = c(3, 4)
) {
  
  # SODMean <- c(18, 18, 18)
  # SODSDV <- c(14, 14, 14)
  # SODFreq <- c(4, 4.5, 4.7)
  # sex <- c("Male", "Male", "Female")
  # grams_ethanol_per_unit <- 8
  # alc_wholly_acute_thresholds <- c(6, 8)
  
  kn <- 600 * 0.9
  grams_ethanol <- 1:kn
  
  # Units consumed above the binge threshold
  
  # if(sex == "Female") {
  #   threshold <- alc_wholly_acute_thresholds[1] # 6 units
  # }
  # 
  # if(sex == "Male") {
  #   threshold <- alc_wholly_acute_thresholds[2] # 8 units
  # }
  
  threshold <- fifelse(sex == "Female", 
                      alc_wholly_acute_thresholds[1], # 6 units
                      alc_wholly_acute_thresholds[2]) # 8 units
  
  # Convert grams of ethanol back to units
  units_vec <- grams_ethanol[1:(kn - 1)] / grams_ethanol_per_unit
  
  # Subtract the threshold and replace negative values with zero
  #units_vec <- units_vec - threshold
  units_vec <- outer(units_vec, threshold, FUN = "-")
  
  units_vec <- replace(units_vec, units_vec < 0, 0)
  
  # Convert the column of vectors back to a matrix
  interval_prob <- matrix(unlist(interval_prob_vec), nrow = kn - 1, ncol = length(SODFreq), byrow = F)
  
  # Calculate the total number of units drunk above the binge threshold
  
  units_above_threshold <-
    matrix(SODFreq, nrow = kn - 1, ncol = length(SODFreq), byrow = T) * # expected number of weekly drinking occasions [number]
    52 * # multiply by the number of weeks in a year [number]
    interval_prob * # the probability that each level of alcohol is consumed on a drinking occasion [vector]
    units_vec # units that are above the threshold
  
  # Total annual units drunk above the binge threshold
  units_above_threshold_sum <- colSums(units_above_threshold)
  
  
  rm(kn,
     grams_ethanol,
     threshold,
     units_vec,
     interval_prob,
     units_above_threshold)
  
  
  return(units_above_threshold_sum)
}





