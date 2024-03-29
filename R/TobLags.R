
#' Tobacco lag times
#'
#' Prepare the disease specific functions that describe how a change in tobacco consumption
#' gradually has an effect on the relative risk of disease incidence over time (up to 40 years)
#' since e.g. someone quit smoking
#'
#' All lag times are taken from a re-analysis of the Cancer prevention II study by \insertCite{Oza2011;textual}{tobalcepi}
#'  and \insertCite{Kontis2014;textual}{tobalcepi}.  
#' The values were sent to us on request by Kontis. Lags are smoothed functions over time describing the proportion of
#' the excess risk due to smoking that still remains.
#'
#' Kontis et al. re-analysed the change in risk after smoking in the ACS-CPS II study from Oza et al.,
#' producing three functions to describe the decline in risk after quitting for each of cancers, CVD and COPD.
#' The estimates were informed by data on former smokers with known quit dates who were disease-free at baseline.
#' The results show the proportion of excess relative risk remaining at each time-point since cessation.
#' A cross-check showed that the figures for cancers were broadly consistent with the findings of the
#' International Agency for Research on Cancer's (IARC)
#' 2007 review of the decline in risk after quitting smoking.
#'
#' The remaining question is how risk declines after quitting smoking for diseases that are not cancers,
#' CVD or COPD. Kontis et al. state that
#' "Randomised trials also indicate that the benefits of behaviour change and pharmacological treatment
#' on diabetes risk occur within a few years, more similar to the CVDs than cancers.
#'  Therefore, we used the CVD curve for diabetes." In-line with Kontis, we apply the rate of decline
#'  in risk of CVD after quitting smoking to type 2 diabetes.
#'   For all remaining conditions we apply the most conservative estimate available 
#'   and assume that the decline in risk follows the cancer estimate provided by Kontis et al., as this has the slowest decline in risk.   
#'
#' @param disease_name Character - the name of the disease under consideration.
#' @param n_years Integer - the number of years from 1 to n over which the effect of a change in
#' consumption emerges. Defaults to 40 years to fit with the current lag data.
#' @param lag_data Data table containing the numerical description of the lag function.
#' The data table "tobacco lag times" is embedded within the stapmr package.
#' @param other_lag_function Character - the name of the lag function to use for tobacco related conditions 
#' that are not categorised as CVD, COPD, or Cancer. Options: c("Cancers", "CVD", "COPD", "immediate"). 
#' The default is "Cancers", which gives the most conservative (i.e. slowest) estimate of the rate of decline in 
#' the risk of disease after quitting smoking.
#'
#' @return Returns a data table with two columns - one for the years since consumption changed, and the other
#' that gives the proportion by which the effect of a change in consumption
#' on an individual's relative risk of disease has so far emerged.
#' 
#' @importFrom data.table := setDT setnames data.table
#' 
#' @export
#' @references
#' \insertRef{Kontis2014}{tobalcepi}  
#' 
#' \insertRef{Oza2011}{tobalcepi}  
#' 
#'
#' @examples
#' \dontrun{
#' TobLags("Pharynx")
#' 
#' TobLags("Low_back_pain", other_lag_function = "immediate")
#' TobLags("Low_back_pain", other_lag_function = "CVD")
#' TobLags("Low_back_pain", other_lag_function = "Cancers")
#'}
TobLags <- function(
    disease_name = c("Pharynx", "Oral_cavity"),
    n_years = 40,
    lag_data = tobalcepi::tobacco_lag_times,
    other_lag_function = "Cancers"
) {
  
  #################################
  # List the specific diseases that fall under each functional form of lag time
  
  cancer_lags <- c("Oral_cavity", "Pharynx", "Lung", "Nasopharynx_sinonasal", "Larynx", "Oesophageal_AC",
                   "Oesophageal_SCC", "Stomach", "Pancreas", "Liver", "Colorectal", "Kidney", "Lower_urinary_tract",
                   "Bladder", "Cervical", "Acute_myeloid_leukaemia")
  
  cvd_lags <- c("Ischaemic_heart_disease", "Haemorrhagic_Stroke", "Ischaemic_Stroke", "Peripheral_arterial_disease",
                "Abdominal_aortic_aneurysm", "Venous_thromboembolism", "Diabetes")
  
  copd_lags <- c("Chronic_obstructive_pulmonary_disease")
  
  
  #################################
  # Specify the functional forms of the lags
  
  ##################
  
  if(!(other_lag_function %in% c("Cancers", "CVD", "COPD", "immediate"))) {
    
    warning("TobLags: specified other_lag_function not in list of possible values")
    
  }
  
  # An instant reduction of risk e.g. for acute conditions
  if(other_lag_function == "immediate") {
    
    lag_func <- c(1, rep(0, n_years))
    
  } else {
    
    # Assume that other diseases follow the cancer lag
    lag_func <- lag_data[cause_group == other_lag_function, excess_risk_percent]
    
  }
  
  ##################
  
  if(disease_name %fin% cancer_lags) {
    lag_func <- lag_data[cause_group == "Cancers", excess_risk_percent]
  }
  
  if(disease_name %fin% cvd_lags) {
    lag_func <- lag_data[cause_group == "CVD", excess_risk_percent]
  }
  
  if(disease_name %fin% copd_lags) {
    lag_func <- lag_data[cause_group == "COPD", excess_risk_percent]
  }
  
  #################################
  # Format the output
  
  # The numbers above are currently in the form of the proportion of excess risk remaining
  # Re-format so they show the cumulative proportion by which risk reduces over time
  # i.e. after 40 years, all excess risk has gone, so the cumulative proportion of risk reduction = 1
  
  disease_lag_data <- data.table(
    time_since_quit = 0:n_years,
    prop_risk_reduction = 1 - lag_func
  )
  
  
  return(disease_lag_data)
}

























