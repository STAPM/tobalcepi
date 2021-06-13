
#' Calculate Population Attributable Fractions \lifecycle{maturing}
#' 
#' Uses \code{RRFunc()} and \code{subgroupRisk()} to 
#' calculate population attributable fractions 
#' based on the survey data provided.
#' 
#' 
#' @param data Data table of individual characteristics
#' @param substance Whether to compute relative risks for just alcohol ("alc"),
#' just tobacco ("tob") or joint risks for tobacco and alcohol ("tobalc").
#' @param tob_include_risk_in_former_smokers Logical - whether the residual risks of smoking in former smokers
#' should be considered (defaults to TRUE).
#' @template alc-epi-args
#' @param use_weights Logical - should the calculation account for survey weights. Defaults to FALSE.
#' Weight variable must be called "wt_int".
#' @param year_range Either an integer vector of the years to be selected or "all". Defaults to "all".
#' @param pool Logical - should the years selected be pooled. Defaults to FALSE.
#' @param subgroups Character vector - the variable names of the subgroups used to stratify the estimates.
#' @param tobalc_include_int Logical - in computing joint relative risks for tobacco and alcohol,
#'  should a (synergistic/multiplicative) interaction between exposure to tobacco and alcohol be included.
#'  Defaults to FALSE. If TRUE, then only interactive effects for oesophageal, pharynx, oral cavity and larynx cancers
#'  are considered.
#' @param within_model Logical - is the function being used to calculate PAFs 
#' from the results of a STAPM model simulation. Defaults to FALSE.
#' @param mort_or_morb Character string - whether the risk functions for conditions with separate mortality and morbidity risk functions 
#' should refer to mortality or morbidity. Values could be "mort" or "morb". Default is "mort".
#' 
#' @return Returns a data.table containing the estimated PAFs.
#' 
#' @importFrom data.table rbindlist setnames
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' tobacco_pafs <- PAFcalc(
#'  data = test_data,
#'  substance = "tob",
#'  tob_include_risk_in_former_smokers = TRUE,
#'  use_weights = TRUE,
#'  year_range = "all",
#'  pool = TRUE,
#'  subgroups = c("sex", "age_cat", "imd_quintile")
#' )
#' 
#' }
#' 
PAFcalc <- function(
  data,
  substance,
  tob_include_risk_in_former_smokers = TRUE,
  alc_protective = TRUE,
  alc_wholly_chronic_thresholds = c(2, 2),
  alc_wholly_acute_thresholds = c(3, 4),
  grams_ethanol_per_unit = 8,
  use_weights = FALSE,
  year_range = "all",
  pool = FALSE,
  subgroups = c("sex", "age_cat"),
  tobalc_include_int = FALSE,
  within_model = FALSE,
  mort_or_morb = c("mort", "morb")[1]
) {
  
  years <- min(data$year):max(data$year)
  
  cat("Assigning relative risks\n")
  
  for(y in years) {
    
    #y <- years[1]
    
    cat("\t", y, "\n")
    
    # Add the relative risks to the data
    data_rr <- tobalcepi::RRFunc(
      data = data[year == y],
      substance = substance,
      tob_diseases = tobalcepi::tob_disease_names,
      tob_include_risk_in_former_smokers = tob_include_risk_in_former_smokers,
      alc_diseases = tobalcepi::alc_disease_names,
      alc_mort_and_morb = c(
        "Ischaemic_heart_disease", 
        "LiverCirrhosis", 
        "Haemorrhagic_Stroke",
        "Ischaemic_Stroke"),
      alc_risk_lags = FALSE,
      alc_protective = alc_protective,
      alc_wholly_chronic_thresholds = alc_wholly_chronic_thresholds,
      alc_wholly_acute_thresholds = alc_wholly_acute_thresholds,
      grams_ethanol_per_unit = grams_ethanol_per_unit,
      show_progress = TRUE,
      within_model = within_model,
      tobalc_include_int = tobalc_include_int)
    
    if(y == years[1]) {
      
      data_rr_comb <- copy(data_rr)
      
    } else {
      
      data_rr_comb <- rbindlist(list(data_rr_comb, copy(data_rr)), use.names = T)
      
    }
    
  }
  
  
  # If need morbidity relative risks
  
  if(mort_or_morb == "morb") {
    data_rr_comb <- data_rr_comb[ , c("Ischaemic_heart_disease", "LiverCirrhosis", "Haemorrhagic_Stroke", "Ischaemic_Stroke") := NULL]
    setnames(data_rr_comb, paste0(c("Ischaemic_heart_disease", "LiverCirrhosis", "Haemorrhagic_Stroke", "Ischaemic_Stroke"), "_morb"), c("Ischaemic_heart_disease", "LiverCirrhosis", "Haemorrhagic_Stroke", "Ischaemic_Stroke"))
  }
  
  
  # Calculate PAFs
  
  if(substance == "tob") disease_names <- tobalcepi::tob_disease_names
  if(substance == "alc") disease_names <- tobalcepi::alc_disease_names
  if(substance == "tobalc") disease_names <- union(tobalcepi::tob_disease_names, tobalcepi::alc_disease_names)
  
  paf_data <- subgroupRisk(
    data = data_rr_comb,
    af = TRUE,
    disease_names = disease_names,
    use_weights = use_weights,
    pool = pool,
    subgroups = subgroups)
  
  
  return(paf_data)
}



