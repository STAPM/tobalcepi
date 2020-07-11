
#' Parameters to estimate amount drunk on single occassions
#' 
#' We use parameter estimates from Hill-McManus et al 2014 -   
#' 
#' Table 3 - Negative binomial regression model for the number of weekly drinking occasions  
#' 
#' Table 5 - Fitted Heckman selection model for probability that an individual drinks on at least 3 separate occasions during the diary period  
#' 
#' Table 6 - Fitted Heckman outcome regression results for the standard deviation in the quantity of alcohol consumed in a drinking occasion   
#' 
#' We do not use parameter estimates from Table 4 - Fitted Tobit regression model for the mean grams of alcohol consumed during a drinking occasion. 
#' This is because we calculate from the data by dividing the weekly mean alcohol consumption by the estimated number of weekly drinking occassions.  
#'
#'
#'
#' @docType data
#'
#' @format A list of three data tables (1 = Table 3, 2 = Table 5, 3 = Table 6)
#'
#' @source Hill-McManus et al 2014. "Estimation of usual occasion-based individual drinking patterns using diary survey data". https://doi.org/https://doi.org/10.1016/j.drugalcdep.2013.09.022. 
#'
#'
#'
#'
#'
"binge_params"
