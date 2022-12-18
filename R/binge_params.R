
#' Parameters to estimate amount drunk on single occasions - England - STAPM version
#' 
#' As our starting point we use the parameter estimates from Hill-McManus et al 2014 - stored within the `tobalcepi` package as the data object `binge_params`. 
#' The problem with using these parameters directly in STAPM is that STAPM does not model the individual life-course trajectories of 
#' some of the covariates investigated by Hill-McManus et al, e.g. income, kids or social status. To get these parameters into a form that can be used in STAPM, 
#' we matched them to the individual covariates in a sample of Health Survey for England data from 2011-2017, and then averaged the parameter values 
#' by age category, sex and IMD quintiles. The code that does this is in the `data-raw/binge_params` folder of the `tobalcepi` package. 
#' Individual height and weight is needed for the calculation of time spent intoxicated, so this is also averaged by age category, sex and IMD quintile.  
#'
#' @docType data
#'
#' @format A list of four data tables (1 = Negative binomial regression model for the number of weekly drinking occasions, 
#' 2 = Fitted Heckman selection model for probability that an individual drinks on at least 3 separate occasions during the diary period, 
#' 3 = Fitted Heckman outcome regression results for the standard deviation in the quantity of alcohol consumed in a drinking occasion, 
#' 4 = average height and weight)
#'
#' @source Hill-McManus et al 2014. "Estimation of usual occasion-based individual drinking patterns using diary survey data". https://doi.org/https://doi.org/10.1016/j.drugalcdep.2013.09.022. 
#'
"binge_params_stapm"

#' Parameters to estimate amount drunk on single occasions - Scotland - STAPM version
#' 
#' Scottish version of `binge_params_stapm` based on the Scottish Health Survey years 2008 to 2019.   
#'
#' @docType data
#'
#' @format A list of four data tables (1 = Negative binomial regression model for the number of weekly drinking occasions, 
#' 2 = Fitted Heckman selection model for probability that an individual drinks on at least 3 separate occasions during the diary period, 
#' 3 = Fitted Heckman outcome regression results for the standard deviation in the quantity of alcohol consumed in a drinking occasion, 
#' 4 = average height and weight)
#'
#' @source Hill-McManus et al 2014. "Estimation of usual occasion-based individual drinking patterns using diary survey data". https://doi.org/https://doi.org/10.1016/j.drugalcdep.2013.09.022. 
#'
"binge_params_stapm_scot"


#' Parameters to estimate amount drunk on single occasions
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
#' This is because we calculate from the data by dividing the weekly mean alcohol consumption by the estimated number of weekly drinking occasions.  
#'
#' @docType data
#'
#' @format A list of three data tables (1 = Table 3, 2 = Table 5, 3 = Table 6)
#'
#' @source Hill-McManus et al 2014. "Estimation of usual occasion-based individual drinking patterns using diary survey data". https://doi.org/https://doi.org/10.1016/j.drugalcdep.2013.09.022. 
#'
"binge_params"



