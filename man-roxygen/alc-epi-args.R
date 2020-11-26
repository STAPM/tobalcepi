#' @param alc_protective Logical - whether to include the protective effects of
#' alcohol in the risk function. Defaults to TRUE. If TRUE, then the part of the risk function < 1 is set to equal 1.
#' @param alc_wholly_chronic_thresholds Numeric vector - the thresholds in UK standard units of alcohol per day 
#' over which individuals begin to experience an elevated risk
#'  for chronic diseases that are wholly attributable to alcohol. Input in the order c(female, male). 
#'  Defaults to the current UK healthy drinking threshold of 14 units/week for females and males, or 2 units/day.  
#' @param alc_wholly_acute_thresholds Numeric vector - the thresholds in UK standard units of alcohol /day over
#'  which individuals begin to experience an elevated risk
#'  for acute diseases that are wholly attributable to alcohol. Input in the form c(female, male). 
#'  Defaults to 3 units/day for females and 4 units/day for males.   
#' @param grams_ethanol_per_unit Numeric value giving the conversion factor for the number of grams of pure
#' ethanol in one UK standard unit of alcohol.  
#' 