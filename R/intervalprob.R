

#' Distribution of single-occasion drinking amount
#'
#' Computes the probability that each integer number of grams of ethanol per day 
#' is consumed on a single drinking occasion.
#'
#' @param grams_ethanol Integer vector - the potential grams of ethanol drunk on a single occasion.
#' @param SODMean Numeric vector - the average amount that each individual is expected to
#' drink on a single drinking occasion.
#' @param SODSDV Numeric vector - the standard deviation of the amount that each individual is expected to
#' drink on a single drinking occasion.
#' @param SODFreq Numeric vector - the expected number of drinking occasions that
#' each individual has each week.
#' @param grams_ethanol_per_unit Numeric value giving the conversion factor for the number of grams of pure
#' ethanol in one UK standard unit of alcohol.
#'
#' @return Returns a numeric vector containing the probability that each amount of alcohol is consumed
#' 
#' @importFrom data.table := setDT setnames
#' 
#' @export
#'
#'
#' 
#'
intervalprob <- function(
  grams_ethanol = 1:540,
  SODMean = NULL,
  SODSDV = NULL,
  SODFreq = NULL,
  grams_ethanol_per_unit = 8
) {
  
  kn <- max(grams_ethanol)
  
  x <- t(vapply(X = grams_ethanol,
                FUN = stats::pnorm, 
                FUN.VALUE = numeric(length(SODMean)),
                mean = SODMean * grams_ethanol_per_unit, # mean
                sd = SODSDV * grams_ethanol_per_unit # variance
  ))
  
  # Convert from the cumulative distribution to the
  # probability that each level of alcohol is consumed on a drinking occasion
  #interval_prob <- x - c(0, x[1:(length(x) - 1)])
  #interval_prob <- diff(x)
  interval_prob <- apply(x, 2, diff)
  
  #interval_prob <- interval_prob / sum(interval_prob)
  
  # compare method against numeric integration
  
  # The line below makes values sum to 1
  # discussion with Alan has concluded that it is needed because 
  # the values are subsequently used in the computation of a weighted average
  interval_prob <- interval_prob / matrix(colSums(interval_prob), nrow = kn - 1, ncol = ncol(interval_prob), byrow = T)
  
  interval_prob[is.na(interval_prob)] <- 0
  
  
  rm(kn, x)
  
  
  return(interval_prob)
}





