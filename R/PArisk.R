

#' Relative risks for alcohol-related injuries \lifecycle{stable}
#'
#' Uses the 'new' binge model methods to calculate a relative risk
#' for each individual for experiencing each cause during one year.
#'
#' This calculation treats an occasion as a single point in time and therefore does not detail
#' about the rate of alcohol absorption (i.e. there is no alcohol absorption rate constant)
#' or the time interval between drinks within an occasion. This could introduce inaccuracies if
#'  e.g. a drinking occasion lasted several hours. The methods to calculate the total time spent intoxicated
#'  (with blood alcohol content greater than zero) are discussed in Taylor et al 2011
#'  and the discussion paper by Hill-McManus 2014. The relative risks for alcohol-related injuries
#'  are taken from Cherpitel et al 2015.
#'
#' @param SODMean Numeric vector - the average amount that each individual is expected to
#' drink on a single drinking occasion.
#' @param SODSDV Numeric vector - the standard deviation of the amount that each individual is expected to
#' drink on a single drinking occasion.
#' @param SODFreq Numeric vector - the expected number of drinking occasions that
#' each individual has each week.
#' @param Weight Numeric vector - each individual's body weight in kg.
#' @param Widmark_r Numeric vector - the fraction of the body mass in which alcohol would be present
#'  if it were distributed at concentrations equal to that in blood.
#'  See examples of use of the Widmark equation in Watson (1981) and Posey and Mozayani (2007).
#' @param cause Character - the acute cause being considered.
#' @param grams_ethanol_per_unit Numeric value giving the conversion factor for the number of grams of pure
#' ethanol in one UK standard unit of alcohol.
#' @param grams_ethanol_per_std_drink Numeric value giving the conversion factor for
#' the number of grams of ethanol in one standard drink.
#' @param liver_clearance_rate_h The rate at which blood alcohol concentration declines (percent / hour).
#' @param getcurve Logical - do you just want to look at the risk function curve?
#'
#' @return Returns a numeric vector of each individual's relative risk of the acute consequences of drinking.
#' 
#' @importFrom data.table := setDT setnames CJ
#' 
#' @export
#'
#'
#' @examples
#' 
#' \dontrun{
#' 
#' ## Further explanation
#'  
#' # For a male with the following characteristics:
#' Weight <- 70 # weight in kg
#' Height <- 2 # height in m
#' Age <- 25 # age in years
#'
#' # We can estimate their r value from the Widmark equation 
#' # using parameter values from Posey and Mozayani (2007)
#' Widmark_r <- 0.39834 + ((12.725 * Height - 0.11275 * Age + 2.8993) / Weight)
#'
#' # They might drink from 1 to 100 grams of ethanol on one occassion
#' grams_ethanol <- 1:100
#'
#' # In minutes, We would expect them to remain intoxicated 
#' # (with blood alcohol content > 0 percent) for
#' Duration_m <- 100 * grams_ethanol / (Widmark_r * Weight * 1000 * (liver_clearance_rate_h / 60))
#'
#' # and hours
#' Duration_h <- Duration_m / 60
#'
#' # Duration is the length of time taken to clear all alcohol from the blood
#' # so we don't apply any thresholds of intoxication,
#' # we just calculate the expected length of time with a bac greater than 0.
#'
#' # Now suppose that on average our example male has 5 drinking occasions per week, and that
#' # on average they drink 3 units of alcohol on an occasion,
#' # and that the standard deviation of amount drunk on an occasion is 14 units.
#'
#' # The cumulative probability distribution of each amount of alcohol being drunk on an occassion is
#' x <- pnorm(grams_ethanol, 2 * 8, 14 * 8)
#'
#' # Convert from the cumulative distribution to the
#' # probability that each level of alcohol is consumed on a drinking occasion
#' interval_prob <- x - c(0, x[1:(length(x) - 1)])
#'
#' # The probability-weighted distribution of time spent intoxicated during a year (52 weeks) is
#' Time_intox <- 5 * 52 * interval_prob * Duration_h
#'
#' # And the expected total time spent intoxicated is
#' Time_intox_sum <- sum(Time_intox)
#'
#' # The relative risk of a transport injury corresponding to each amount drunk on a single occasion
#' # corresponds to the number of standard drinks consumed
#'
#' # We convert to standard drinking and apply the risk parameters from Cherpitel
#'
#' v <- grams_ethanol / 12.8
#' v1 <- (v + 1) / 100
#'
#' # Parameters from Cherpitel
#' b1 <- 3.973538882
#' b2 <- 6.65184e-6
#' b3 <- 0.837637
#' b4 <- 1.018824
#'
#' # Apply formula for the risk curve from Cherpital
#' lvold_1 <- log(v1) + b1
#' lvold_2 <- (v1^3) - b2
#' logitp <- lvold_1 * b3 + lvold_2 * b4
#' p <- boot::inv.logit(logitp)
#'
#' # The relative risk associated with each amount drunk on an occasion
#' rr <- p / p[1]
#'
#' # The relative risk multiplied by the time exposed to that level of risk
#' Current_risk <- rr * Time_intox
#'
#' # The sum of the relative risk associated with the time spent intoxicated during one year
#' Risk_sum <- sum(Current_risk)
#'
#' # The average annual relative risk, considering that time in the year spent with a
#' # blood alcohol content of zero has a relative risk of 1.
#' Annual_risk <- min(
#'   (Risk_sum + 1 * (365 * 24 - Time_intox_sum)) / (365 * 24),
#'   365 * 24, na.rm = T)
#'
#'
#'
#' # THE FOLLOWING ARE NOT CONSIDERED IN THIS CALCULATION
#'
#' # Elapsed time in minutes since consuming alcohol
#' t <- 30
#'
#' # Alcohol absorbtion rate constant
#' k_empty_stomach <- 6.5 / 60 # grams of ethanol per minute
#'
#' # Alcohol absorbtion
#' alcohol_absorbed <- grams_ethanol * (1 - exp(-k_empty_stomach * t))
#'
#' # Calculate blood alcohol content using the Widemark eqn
#' bac <- (100 * alcohol_absorbed / (Widmark_r * Weight * 1000)) - ((liver_clearance_rate_h / 60) * t)
#' }
#'
PArisk <- function(
  SODMean = NULL,
  SODSDV = NULL,
  SODFreq = NULL,
  Weight = NULL,
  Widmark_r = NULL,
  cause = "Transport",
  grams_ethanol_per_unit = 8,
  grams_ethanol_per_std_drink = 12.8,
  liver_clearance_rate_h = 0.017,
  getcurve = FALSE
) {
  
  kn <- 600
  
  grams_ethanol <- 1:kn
  
  # The amounts of alcohol (g ethanol) that could be consumed on an occasion
  # i.e. the mass of alcohol ingested
  #grams_ethanol <- 1:100 # units * ConvertToGramOfAlcohol#1:100
  
  if(!isTRUE(getcurve)) {
    
    # Duration is calculated in minutes
    
    # Convert liver clearance rate from per hour to per minute
    liver_clearance_rate_m <- liver_clearance_rate_h / 60
    
    #Duration_m <- 100 * grams_ethanol[1:(kn - 1)] / (Widmark_r * Weight * 1000 * liver_clearance_rate_m)
    
    
    # Look for faster vectorised alternatives to outer
    
    x <- 100 * grams_ethanol[1:(kn - 1)]
    y <- Widmark_r * Weight * 1000 * liver_clearance_rate_m
    
    # x <- 1:600
    # y <- 1:1e5
    
    Duration_m <- outer(x, y, FUN = "/")
    
    
    # Convert to hours
    Duration_h <- Duration_m / 60
    
    #######################
    # Calculate the cumulative probability distribution of each amount of alcohol (1 to 100 g) being drunk on an occasion
    # x <- stats::pnorm(
    #   grams_ethanol,
    #   SODMean * grams_ethanol_per_unit, # mean
    #   SODSDV * grams_ethanol_per_unit # variance
    # )
    
    # grams_ethanol <- 1:600
    # SODMean <- 4
    # SODSDV <- 2
    # grams_ethanol_per_unit <- 8
    # lb <- bench::mark(
    # x <- t(sapply(grams_ethanol,
    #               stats::pnorm, 
    #               mean = SODMean * grams_ethanol_per_unit, # mean
    #               sd = SODSDV * grams_ethanol_per_unit # variance
    # ))
    # ,
    # 
    
    x <- t(vapply(X = grams_ethanol,
                  FUN = stats::pnorm, 
                  FUN.VALUE = numeric(length(SODMean)),
                  mean = SODMean * grams_ethanol_per_unit, # mean
                  sd = SODSDV * grams_ethanol_per_unit # variance
    ))
    
    # 
    # )
    # 
    # lb
    
    #######################
    # Convert from the cumulative distribution to the
    # probability that each level of alcohol is consumed on a drinking occasion
    #interval_prob <- x - c(0, x[1:(length(x) - 1)])
    #interval_prob <- diff(x)
    interval_prob <- apply(x, 2, diff)
    
    #interval_prob <- interval_prob / sum(interval_prob)
    
    # NOT SURE IF THE LINE BELOW IS NEEDED
    # the code makes values sum to 1
    # discussion with Alan has concluded that it is needed because 
    # the values are subsequently used in the computation of a weighted average
    interval_prob <- interval_prob / matrix(colSums(interval_prob), nrow = kn - 1, ncol = ncol(interval_prob), byrow = T)
    
    interval_prob[is.na(interval_prob)] <- 0
    
    
    #######################
    # Calculate the total annual time spent intoxicated
    # here we use 'intoxicated' to mean having a bac > 0
    # freq_drinks * 52 * interval_prob * duration
    
    Time_intox <-
      
      # be careful about how the SODFreq vector is oriented by row or by col
      # should be by row as each column is an individual
      matrix(SODFreq, nrow = kn - 1, ncol = length(SODFreq), byrow = T) * #SODFreq * # expected number of weekly drinking occasions [number]
      
      
      52 * # multiply by the number of weeks in a year [number]
      interval_prob * # the probability that each level of alcohol is consumed on a drinking occasion [vector]
      Duration_h # the duration of intoxication (1 to 100g) for each amount of alcohol that could be drunk [vector]
    
    # Total annual time spent intoxicated over all levels of consumption
    Time_intox_sum <- colSums(Time_intox)
    
  }
  
  #######################
  # Apply risk function
  
  # all risk functions from Cherpitel et al 2015
  
  # NOTE THAT VOLUME IS IN STANDARD DRINKS, NOT GRAMS, PER OCCASION. 1 STD. DRINK = 16ml (12.8g) OF ETHANOL
  
  v <- grams_ethanol[1:(kn - 1)] / grams_ethanol_per_std_drink
  
  v1 <- (v + 1) / 100
  
  # Traffic
  if(cause == "Transport") {
    
    b1 <- 3.973538882
    b2 <- 6.65184e-6
    b3 <- 0.837637
    b4 <- 1.018824
    
    lvold_1 <- log(v1) + b1
    lvold_2 <- v1^3 - b2
    logitp <- lvold_1 * b3 + lvold_2 * b4
    p <- exp(logitp) / (1 + exp(logitp))
    #p <- boot::inv.logit(logitp)
    #or <- (p / (1 - p)) / (p[1] / (1 - p[1]))
    rr <- p / p[1]
    
  }
  
  # Violence
  if(cause == "Violence") {
    
    b1 <- 5.084489629
    b2 <- 0.0000578783
    b3 <- 0.42362
    b4 <- 0.562549
    
    lvold_1 <- v1^-0.5 - b1
    lvold_2 <- v1^3 - b2
    logitp <- lvold_1 * -b3 + lvold_2 * b4
    p <- exp(logitp) / (1 + exp(logitp))
    #p <- boot::inv.logit(logitp)
    #or <- (p / (1 - p)) / (p[1] / (1 - p[1]))
    rr <- p / p[1]
    
  }
  
  # Fall
  if(cause == "Fall") {
    
    b1 <- 0.1398910338
    b2 <- 0.0195695013
    b3 <- 17.84434
    b4 <- 17.6229
    
    lvold_1 <- v1^0.5 - b1
    lvold_2 <- v1 - b2
    logitp <- lvold_1 * b3 + lvold_2 * -b4
    p <- exp(logitp) / (1 + exp(logitp))
    #p <- boot::inv.logit(logitp)
    #or <- (p / (1 - p)) / (p[1] / (1 - p[1]))
    rr <- p / p[1]
    
  }
  
  # Other
  if(cause == "Other") {
    
    b1 <- 7.965292902
    b2 <- 0.015761462
    b3 <- 0.28148
    b4 <- 2.00946
    
    lvold_1 <- v1^-0.5 - b1
    lvold_2 <- v1 - b2
    logitp <- lvold_1 * -b3 + lvold_2 * -b4
    p <- exp(logitp) / (1 + exp(logitp))
    #p <- boot::inv.logit(logitp)
    #or <- (p / (1 - p)) / (p[1] / (1 - p[1]))
    rr <- p / p[1]
    
  }
  
  if(!isTRUE(getcurve)) {
    
    # Risk at that level of grams
    Current_risk <- rr * Time_intox
    
    # Total risk
    #Risk_sum <- sum(Current_risk)
    Risk_sum <- colSums(Current_risk)
    
    # Annual risk
    #Annual_risk <- min((Risk_sum + 1 * (365 * 24 - Time_intox_sum)) / (365 * 24), 365 * 24, na.rm = T)
    Annual_risk <- vapply(
      X = 1:length(Risk_sum), 
      FUN = function(z) {
        min((Risk_sum[z] + 1 * (365 * 24 - Time_intox_sum[z])) / (365 * 24), (365 * 24), na.rm = T)
      },
      FUN.VALUE = numeric(1)
    )
    
    # 
    # rm(
    #   grams_ethanol,
    #   v, v1, logitp, p
    # )
    # gc()
    # 
    # 
    
    
    return(Annual_risk)
    
  } else {
    
    # Relative risk curve
    return(rr)
    
  }
}





