
#' Summarise relative risk
#'
#' Calculate the average relative risk across individuals in a subgroup,
#' or calculate the subgroup specific attributable fraction based on the current relative risks.
#'
#' Attributable fractions are calculated using the method as in Bellis & Jones 2014, which is also equivalent to the
#' method described in the Brennan et al. 2015 SAPM mathematical description paper.
#'
#' @param data A data table of individual characteristics.
#' @param label Character - a label to append to the outcome variable to help identify it in later calculations.
#' @param disease_names Character vector - the names of the diseases for which summaries of relative risk are required.
#' @param af Logical - if TRUE, then attributable fractions are calculated. If FALSE, then the total relative risk
#'  is calculated. Defaults to FALSE.
#' @param use_weights Logical - should the calculation account for survey weights. Defaults to FALSE.
#' Weight variable must be called "wt_int".
#' @param year_range Either an integer vector of the years to be selected or "all". Defaults to "all".
#' @param pool Logical - should the years selected be pooled. Defaults to FALSE.
#' @param subgroups Character vector - the variable names of the subgroups used to stratify the estimates.
#' @param mort_or_morb Character - for alcohol related diseases that have separate
#' relative risk curves for mortality and morbidity, should the risks corresponding to
#'  mortality ("mort") or morbidity ("morb") be used.
#' @param alc_mort_and_morb Character vector of the names of the 
#' alcohol related diseases that have separate risk functions for
#' mortality and morbidity.
#' @param substance Whether to compute relative risks for just alcohol ("alc"),
#' just tobacco ("tob") or joint risks for tobacco and alcohol ("tobalc").
#' @param smooth Logical - should the age patterns in average risk be smoothed with a moving average. 
#' For use only if average risk is stratified by single years of age. Defaults to FALSE
#' @param oesoph_subtypes Logical - should the attributable fractions for oesophageal cancer 
#' be multiplied by the proportions of each subtype. Defaults to FALSE.
#'
#' @return Returns a data table containing the subgroup specific summaries for each disease.
#' 
#' @importFrom data.table := setDT setnames copy .SD .N
#' 
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' # Simulate individual data
#'
#' # Using the parameters for the Gamma distribution from Kehoe et al. 2012
#' n <- 1e4
#' grams_ethanol_day <- rgamma(n, shape = 0.69, scale = 19.03)
#'
#' data <- data.table(
#'   year = 2016,
#'   weekmean = grams_ethanol_day * 7 / 8,
#'   peakday = 2 * grams_ethanol_day / 8,
#'   age = rpois(n, 30),
#'   sex = sample(x = c("Male", "Female"), size = n, replace = T),
#'   income5cat = "1_lowest income",
#'   imd_quintile = "5_most_deprived",
#'   kids = "0",
#'   social_grade = "C2DE",
#'   eduend4cat = "16-18", # age finished education
#'   ethnic2cat = "white", # white / non-white
#'   employ2cat = "yes", # employed / not
#'   wtval = rnorm(n, mean = 60, sd = 5), # weight in kg
#'   htval = rnorm(n, mean = 1.7, sd = .1) # height in m
#' )
#'
#' # Disease names
#' alc_disease_names <- c(
#'   "Pharynx",
#'   "Ischaemic_heart_disease",
#'   "LiverCirrhosis",
#'   "Transport_injuries",
#'   "Alcohol_poisoning",
#'   "Alcoholic_gastritis"
#' )
#'
#' # Run basic function without alcohol lags
#' test_data <- RRFunc(
#'   data = copy(data),
#'   substance = "alc",
#'   alc_diseases = alc_disease_names,
#'   alc_wholly_chronic_thresholds = c(2, 2),
#'   alc_wholly_acute_thresholds = c(3, 3),
#'   show_progress = TRUE
#' )
#'
#' # Calculate alcohol attributable fractions
#' test_aafs <- subgroupRisk(
#'   data = test_data$data_plus_rr,
#'   disease_names = alc_disease_names,
#'   af = TRUE,
#'   subgroups = "sex"
#' )
#'
#' test_aafs
#' }
subgroupRisk <- function(
    data,
    label = NULL,
    disease_names = c("Pharynx", "Oral_cavity"),
    af = FALSE,
    use_weights = FALSE,
    year_range = "all",
    pool = FALSE,
    subgroups = c("sex", "age_cat"),
    mort_or_morb = "mort",
    alc_mort_and_morb = c("Ischaemic_heart_disease", "LiverCirrhosis", "Haemorrhagic_Stroke", "Ischaemic_Stroke"),
    substance = c("alc", "tob", "tobalc")[3],
    smooth = FALSE,
    oesoph_subtypes = FALSE
) {
  
  out <- copy(data) # see if this can be removed from code as inefficient
  
  # To switch-out the mortality risk functions for the morbidity risk functions if necessary
  if(substance %fin% c("alc", "tobalc") & mort_or_morb == "morb" & !is.null(alc_mort_and_morb)) {
    
    out[ , (alc_mort_and_morb) := NULL]
    
    for(i in alc_mort_and_morb) {
      setnames(out, paste0(i, "_morb"), stringr::str_remove(i, "_morb"))
    }
    
  }
  
  if("age_cat" %in% subgroups & !("age_cat" %in% colnames(out))) {
    out[ , age_cat := c("12-15", "16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")[findInterval(age, c(-10, 16, 18, 25, 35, 50, 65, 75))]]
  }
  
  # To select a specified range of years of data
  if(year_range[1] != "all") {
    out <- out[year %in% year_range]
  }
  
  # If several years of data are selected, should they be pooled
  if(pool == T) {
    out[ , year := 1]
  }
  
  # Create a weighting variable
  # depending on whether survey weights are to be used or not
  if(use_weights == F) {
    out[, weight := 1 / .N, by = c(subgroups, "year")]
  } else {
    out[, weight := wt_int / sum(wt_int, na.rm = T), by = c(subgroups, "year")]
  }
  
  
  # List of diseases for which absolute rather than relative risk is used
  # These are all the wholly attributable acute and chronic conditions for alcohol
  abs_diseases <- c(
    "Excessive_Blood_Level_of_Alcohol",
    "Toxic_effect_of_alcohol",
    "Alcohol_poisoning",
    "Evidence_of_alcohol_involvement_determined_by_blood_alcohol_level",
    "Acute_intoxication",
    "Alcoholic_cardiomyopathy",
    "Alcoholic_gastritis",
    "Alcoholic_liver_disease",
    "Acute_pancreatitis_alcohol_induced",
    "Chronic_pancreatitis_alcohol_induced",
    "Alcohol_induced_pseudoCushings_syndrome",
    "Alcoholic_myopathy",
    "Alcoholic_polyneuropathy",
    "Maternal_care_for_suspected_damage_to_foetus_from_alcohol",
    "Degeneration",
    "Mental_and_behavioural_disorders_due_to_use_of_alcohol")
  
  # Standardise the relative risks by subtracting 1 and multiplying by the weight
  for (d in disease_names) {
    
    kn <- NROW(out[is.na(get(d)) | is.infinite(get(d))])
    testthat::expect_equal(kn, 0, info = paste0("subgroupRisk: NA or infinite values in risk values for ", d, " input into function"))
    
    kn <- NROW(out[get(d) < 0])
    testthat::expect_equal(kn, 0, info = paste0("subgroupRisk: negative values in risk values for ", d, " input into function"))
    
    
    # NEEDS MORE CHECKING TO ENSURE CONSISTENT WITH THE MATHEMATICS
    
    
    if(isTRUE(af)) {
      
      # For PAF
      out[, (paste0(d, "_z")) := weight * (get(d) - 1)]
      #out[, (paste0(d, "_z")) := weight * (get(d))]
      
    } else {
      
      # For PIF
      out[, (paste0(d, "_z")) := weight * get(d)]
      
    }
    
    
  }
  
  ############################################################
  # To prepare for subsequent computation of a PIF
  # compute the average relative risk within a subgroup
  
  # compute the average rather than the total, so that when we later calculate the ratio
  # of this aggregated relative risk between treatment and control arms,
  # the ratio is not influenced by differences in the number of individuals
  # i.e. we want to calculate the ratio of the expected value of individual risk in each arm
  
  if(!isTRUE(af)) {
    
    if(!("year" %in% subgroups) & "year" %in% colnames(out)) {
      subgroups <- c(subgroups, "year")
    }
    
    # calculate average relative risk
    out_risk <- out[,
                    lapply(.SD, function(x) {
                      sum(x, na.rm = T)
                    }),
                    by =  subgroups,
                    .SDcols = paste0(disease_names, "_z")]
    
    # if(any(disease_names %in% colnames(out))) {
    #   out[ , (disease_names[disease_names %in% colnames(out)]) := NULL]
    # }
    
    setnames(out_risk, paste0(disease_names, "_z"), disease_names)
    
    
    
    out_risk <- melt(
      out_risk,
      id.vars = subgroups,
      variable.name = "condition",
      value.name = paste0("av_risk_", label)
    )
    
    kn <- NROW(out_risk[is.na(get(paste0("av_risk_", label))) | is.infinite(get(paste0("av_risk_", label)))])
    testthat::expect_equal(kn, 0, info = paste0("subgroupRisk: NA or infinite values in average risk values for ", d, " after calculation"))
    
    kn <- NROW(out_risk[get(paste0("av_risk_", label)) < 0])
    testthat::expect_equal(kn, 0, info = paste0("subgroupRisk: negative values in average risk values for ", d, " after calculation"))
    
    # If the average relative risks should be smoothed over age
    # for use when using single years of age
    if(isTRUE(smooth)) {
      
      out_risk[ , (paste0("av_risk_", label)) := {
        
        kn <- 7 # the width in years of the moving average age window
        
        z <- TTR::SMA(get(paste0("av_risk_", label)), n = kn)
        
        z <- z[!is.na(z)]
        
        z[z < 0] <- 0
        
        c(rep(z[1], kn - 1), z)
        
      }
      , by = c(subgroups[subgroups != "age"], "condition")]
      
    }
  }
  
  ############################################################
  # For attributable fractions
  
  if(isTRUE(af)) {
    
    
    if(!("year" %in% subgroups) & "year" %in% colnames(out)) {
      subgroups <- c(subgroups, "year")
    }
    
    # calculate attributable fractions, considering residual risk in former smokers
    out_risk <- out[,
                    lapply(.SD, function(x) {
                      sum(x, na.rm = T) / (sum(x, na.rm = T) + 1)
                    }),
                    by =  subgroups,
                    .SDcols = paste0(disease_names, "_z")]
    
    # if(any(disease_names %in% colnames(out_risk))) {
    #   out_risk[ , (disease_names[disease_names %in% colnames(out_risk)]) := NULL]
    # }
    
    ##########################
    if(oesoph_subtypes == TRUE) {
      
      # Summarise the proportion of oesophageal cancer cases that are scc 
      # in the same way as the above attributable fractions
      out_scc_prop <- out[ , .(prop_scc = sum(weight * prop_scc, na.rm = T) / sum(weight, na.rm = T)), by =  subgroups]
      
      # Merge in the proportions of scc into the paf dataset
      out_risk <- merge(out_risk, out_scc_prop, all.x = T, all.y = F, by = subgroups)
      
      # Multiple the estimated attributable fractions for Oesophageal Scc and AC by the proportions of each subtype
      out_risk[ , Oesophageal_SCC_z := Oesophageal_SCC_z * prop_scc]
      
      if(substance %fin% c("tob")) {
        
        out_risk[ , Oesophageal_AC_z := Oesophageal_AC_z * (1 - prop_scc)]
        
      }
    }
    ###########################
    
    setnames(out_risk, paste0(disease_names, "_z"), disease_names)
    
    
    out_risk <- melt(
      out_risk,
      id.vars = subgroups,
      variable.name = "condition",
      value.name = "af"
    )
    
    # Set the AAF = 1 for wholly attributable conditions
    out_risk[condition %in% abs_diseases, af := 1]
    
  }
  
  
  return(out_risk)
}


