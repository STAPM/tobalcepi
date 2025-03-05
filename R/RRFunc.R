
#' Individual relative risks of disease
#'
#' @description 
#' This function takes a sample of individuals and computes each individual's relative risk
#' for each disease according to their current tobacco and alcohol consumption. 
#' There is an option to tailor this
#' to the alcohol only, tobacco only, or joint tobacco and alcohol contexts. 
#'
#' @details See below
#' @section Alcohol risk:
#' For alcohol, the relative risk for each individual for each disease is calculated based on 
#' their average weekly alcohol consumption (using [tobalcepi::RRalc()]). 
#' Alcohol consumption is converted to grams of ethanol consumed on average in a day, and 
#' this is truncated at 150g/day. We assume 8 grams of ethanol per UK standard unit of alcohol. 
#'  For diseases that have separate mortality and morbidity risk functions, 
#'  separate variables are created containing
#'  the relative risks for each for the same disease.
#' Unlike tobacco, there is no "former drinker" state in our alcohol modelling, meaning that 
#' individuals are not recorded as being former drinkers -- 
#' instead their alcohol consumption just falls to zero and their
#' relative risk for disease changes accordingly.   
#'
#' @section Alcohol lags:
#' To account for the lagged effects of changes to the amount that individuals drink on their
#' current risk of disease, 
#' it is necessary to add memory to our modelling, 
#' which we do in this function by storing each individual's 
#' past trajectory of the relative risk that they were assigned 
#' for each disease. Doing so adds extra computations and makes the model run a bit slower. 
#' In each year of the simulation, the current relative risk of an individual is 
#' adjusted to take account of 
#' each individual's stored drinking histories. 
#' This adjustment takes the form of a weighted average of current and past relative risk, 
#' where the weights are proportional to
#' the disease-specific lag function (which comes from \code{\link{AlcLags}}). 
#' This method is slightly different to the method that was developed for SAPM, 
#' as it needed to be adapted to suit the modelling of individual life-course 
#' trajectories of alcohol consumption.  
#'
#' @section Tobacco risk:
#' For tobacco, the relative risk for each individual is calculated based on whether 
#' they are a current, former or never smoker (using \code{\link{RRtob}}).
#' Currently, all current smokers have the same relative risk regardless of 
#' the amount they currently smoke or have smoked in the past (but we are in the process 
#' of developing inputs and a function to take account of dose-response effects of 
#' the amount currently smoked using \code{\link{RRTobDR}}).       
#'
#' @section Tobacco lags:
#' Former smokers are initially given the relative risk associated with current smokers (using \code{\link{RRtob}}), 
#' which we then scale according to a disease-specific
#' function that describes how risk declines after quitting smoking (which comes from \code{\link{TobLags}}). 
#' After 40 years from quitting, we assume that risk has reached the level of a never smoker.     
#'
#' @section Joint alcohol and tobacco risk:
#' If both tobacco and alcohol are being considered in a joint model,
#' we combine the relative risks for current drinkers and smokers. 
#' In implementing this combination of risks, we have built-in the option 
#' to take account of synergistic effects (i.e. when the combined 
#' risk from tobacco and alcohol consumption is more that would be expected 
#' from the additive combination of risks, because for some conditions that 
#' tobacco and alcohol consumption interact physiologically, and that interaction 
#' further increases disease risk). 
#' We currently include estimates of synergistic effects for 
#' oral, pharyngeal, laryngeal and oesophageal cancers. 
#' We apply these effects using \code{\link{TobAlcInt}} by scaling the joint risks by a 'synergy index', 
#' which takes the result of a meta-analysis of the additional
#' risk faced by people because they consume both tobacco and alcohol.   
#'
#' @param data Data table of individual characteristics - 
#' this function uses current smoking and drinking status/amount.
#' @param substance Whether to compute relative risks for just alcohol ("alc"),
#' just tobacco ("tob") or joint risks for tobacco and alcohol ("tobalc").
#' @param k_year Integer giving the current year of the simulation. Defaults to NULL.
#' @param alc_diseases Character vector of alcohol related diseases.
#' @param alc_mort_and_morb Character vector of the names of the 
#' alcohol related diseases that have separate risk functions for
#' mortality and morbidity.
#' @param alc_risk_lags Logical - should each individual's relative risks 
#' for alcohol be lagged according to
#' their past individual life-course trajectory of relative risks. 
#' Defaults to FALSE. 
#' This should only be set to TRUE for a model run that simulates individual trajectories,
#' and should be FALSE if this function is being used 
#' as part of the calculation of population attributable fractions.
#' @param alc_indiv_risk_trajectories_store Data table that stores 
#' each individual's life-course history of relative risks for alcohol related diseases. 
#' This can be NULL for the first year of the simulation, and if this is the case then the 
#' function will initialise and return this storage data table after the first year of the simulation.
#' @template alc-epi-args
#' @param tob_diseases Character vector of tobacco related diseases.
#' @param tob_include_risk_in_former_smokers Logical - whether the residual risks of smoking in former smokers
#' should be considered (defaults to TRUE).
#' @param tobalc_include_int Logical - in computing joint relative risks for tobacco and alcohol,
#'  should a (synergistic/multiplicative) interaction between exposure to tobacco and alcohol be included.
#'  Defaults to FALSE. If TRUE, then only interactive effects for oesophageal, pharynx, oral cavity and larynx cancers
#'  are considered.
#' @param tobalc_int_data Data table containing the disease-specific interactions between tobacco and alcohol.
#' @param show_progress Logical - Should the progress of the loop through diseases be shown. Defaults to FALSE.
#' @param within_model Logical - is the function being used within a STAPM simulation. 
#' Defaults to TRUE. This is used only to determine which version of the alcohol binge model function to use - 
#' there is a version that suits the STAPM model by using only the age, sex and IMD quintile variables 
#' that are tracked within the STAPM model simulation.
#' @param country Character string - "England" or "Scotland"
#' @param other_lag_function Character - the name of the lag function to use for tobacco related conditions 
#' that are not categorised as CVD, COPD, or Cancer. Options: c("Cancers", "CVD", "COPD", "immediate"). 
#' The default is "Cancers", which gives the most conservative (i.e. slowest) estimate of the rate of decline in 
#' the risk of disease after quitting smoking.
#' 
#' @return Two data tables are returned:
#' \itemize{
#' \item "data_plus_rr" is a version of "data" with added columns that give each
#' individual's relative risk for each disease.
#' \item "new_alc_indiv_risk_trajectories_store" is a version of "alc_indiv_risk_trajectories_store" with
#' the relative risks for the current year added to the store.
#' }
#' 
#' 
#' @importFrom data.table := setDT setnames copy rbindlist
#' 
#' @export
#' 
#' @seealso \code{\link{RRalc}} for alcohol-specific risks, \code{\link{RRtob}} for tobacco-specific risks, 
#' \code{\link{AlcLags}} for alcohol-specific lag times, and \code{\link{TobLags}} for tobacco-specific lag times.
#' 
#'
#' @examples
#' \dontrun{
#' #############################
#' ## ALCOHOL
#'
#' # Simulate individual data
#'
#' # Using the parameters for the Gamma distribution from Kehoe et al. 2012
#' n <- 1e4
#' grams_ethanol_day <- rgamma(n, shape = 0.69, scale = 19.03)
#'
#' # Note: the socioeconomic and other variables are needed for the binge model
#'
#' data <- data.table(
#'   year = 2016,
#'   weekmean = grams_ethanol_day * 7 / 8,
#'   #peakday = 2 * grams_ethanol_day / 8,
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
#' # Add individual ids to the data
#' data <- MakeSeeds(data, n = 0)
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
#' test_data <- copy(data)
#'
#' test_data1 <- RRFunc(
#'   data = test_data,
#'   substance = "alc",
#'   k_year = 2017,
#'   alc_diseases = alc_disease_names,
#'   alc_indiv_risk_trajectories_store = NULL,
#'   alc_wholly_chronic_thresholds = c(2, 2),
#'   alc_wholly_acute_thresholds = c(3, 4),
#'   show_progress = TRUE
#' )
#'
#' test_data1
#'
#' test_data <- copy(data)
#' test_data[ , year := 2017]
#'
#' test_data2 <- RRFunc(
#'   data = test_data,
#'   substance = "alc",
#'   k_year = 2018,
#'   alc_diseases = alc_disease_names,
#'   alc_indiv_risk_trajectories_store = test_data1$new_alc_indiv_risk_trajectories_store,
#'   alc_wholly_chronic_thresholds = c(2, 2),
#'   alc_wholly_acute_thresholds = c(3, 4),
#'   show_progress = TRUE
#' )
#'
#' test_data2
#'
#'
#' #############################
#' ## TOBACCO
#'
#' tob_disease_names <- c(
#'   "Pharynx",
#'   "Chronic_obstructive_pulmonary_disease",
#'   "Ischaemic_heart_disease",
#'   "Lung",
#'   "Influenza_clinically_diagnosed",
#'   "Diabetes",
#'   "Schizophrenia"
#' )
#'
#' n <- 1e4
#'
#' data <- data.table(
#'   smk.state = sample(x = c("current", "former", "never"), size = n, replace = T),
#'   time_since_quit = sample(x = 0:40, size = n, replace = T),
#'   age = rpois(n, 30),
#'   sex = sample(x = c("Male", "Female"), size = n, replace = T)
#' )
#'
#' data[smk.state != "former", time_since_quit := NA]
#'
#' # Tobacco relative risks for Pharygeal cancer
#' RRFunc(
#'   data = data,
#'   substance = "tob",
#'   tob_diseases = tob_disease_names,
#'   show_progress = TRUE,
#'   other_lag_function = "Cancers"
#'   
#' )
#'
#'
#' #############################
#' ## TOBACCO AND ALCOHOL
#'
#' }
RRFunc <- function(
    data,
    substance = c("tob", "alc", "tobalc"),
    k_year = NULL,
    alc_diseases = tobalcepi::alc_disease_names,
    alc_mort_and_morb = c("ischaemic_heart_disease", "livercirrhosis", "haemorrhagic_stroke", "ischaemic_stroke"),
    alc_risk_lags = TRUE,
    alc_indiv_risk_trajectories_store = NULL,
    alc_protective = TRUE,
    alc_wholly_chronic_thresholds = c(2, 2),
    alc_wholly_acute_thresholds = c(3, 4),
    grams_ethanol_per_unit = 8,
    tob_diseases = tobalcepi::tob_disease_names,
    tob_include_risk_in_former_smokers = TRUE,
    tobalc_include_int = FALSE,
    tobalc_int_data = tobalcepi::tob_alc_risk_int,
    show_progress = FALSE,
    within_model = TRUE,
    country = c("England", "Scotland")[1],
    other_lag_function = "Cancers"
) {
  
  
  #########################################
  # Organise disease lists
  
  alc_diseases_expanded <- c(alc_diseases, paste0(alc_mort_and_morb, "_morb"))
  
  alc_lag_diseases <- c("Pharynx", "Oral_cavity", "Oesophageal_SCC", "Colorectal", "Liver",
                        "Larynx", "Pancreas", "Breast", "Alcohol_induced_pseudoCushings_syndrome", "Degeneration", "Alcoholic_polyneuropathy",
                        "Alcoholic_myopathy", "Alcoholic_cardiomyopathy", "Maternal_care_for_suspected_damage_to_foetus_from_alcohol",
                        "LiverCirrhosis", "Chronic_Pancreatitis", "Acute_Pancreatitis",
                        "Acute_pancreatitis_alcohol_induced", "Chronic_pancreatitis_alcohol_induced",
                        "Alcoholic_liver_disease", "Diabetes", "HypertensiveHeartDisease", "Cardiac_Arrhythmias", 
                        "Ischaemic_heart_disease", "Haemorrhagic_Stroke", "Ischaemic_Stroke", "Epilepsy", "Alcoholic_gastritis", 
                        "Tuberculosis", "Influenza_clinically_diagnosed",
                        "Influenza_microbiologically_confirmed", "Pneumonia", paste0(alc_mort_and_morb, "_morb"))
  
  if(substance == "alc") {
    
    # For the diseases that have separate risk functions for mortality and morbidity
    # expand the list of diseases so that the mortality and morbidity versions
    # are included as separate variables
    
    # Set the default as mortality
    # and mark the additions to the disease list with the postscript "_morb"
    diseases <- alc_diseases_expanded
    
  }
  
  if(substance == "tob") {
    diseases <- tob_diseases
  }
  
  if(substance == "tobalc") {
    diseases <- union(alc_diseases_expanded, tob_diseases)
  }
  
  dn <- length(diseases)
  
  
  #########################################
  # Extra setup for alcohol related risk
  
  #data_temp <- copy(data)
  #data1 <- tobalcepi::AlcBinge_stapm(data_temp)
  #data2 <- tobalcepi::AlcBinge(data_temp)
  
  
  if(substance %in% c("alc", "tobalc")) {
    
    if(isTRUE(within_model)) {
      
      # Calculate 'binge model' parameters
      
      # Estimate the characteristics of single occasion drinking
      # based on the coefficients from Hill-McManus et al 2014
      
      # new version of function - adapted to new STAPM modelling
      
      if(country == "England") {
        binge_params_sim <- tobalcepi::binge_params_stapm
      }
      
      if(country == "Wales") {
        binge_params_sim <- tobalcepi::binge_params_stapm_wales ### DAMON EDIT: USING ENGLAND FOR NOW (17/02/2025)
      }
      
      if(country == "Scotland") {
        binge_params_sim <- tobalcepi::binge_params_stapm_scot
      }
      
      data <- tobalcepi::AlcBinge_stapm(
        data = data, 
        params = binge_params_sim)
      
    } else {
      
      # original version of function - replicated from SAPM modelling
      data <- tobalcepi::AlcBinge(data)
      
    }
    
    # Convert units to grams of alcohol / truncate
    data[ , GPerDay := weekmean * grams_ethanol_per_unit / 7]
    data[GPerDay >= 150, GPerDay := 150]
    #data[ , peakday_grams := peakday * grams_ethanol_per_unit]
    
    # for model running efficiency,
    # use the binge model parameters to calculate a column of vectors
    # for the probability that each individual will drink each amount of grams of ethanol on a single drinking occasion
    
    sod_probs <- tobalcepi::intervalprob(grams_ethanol = 1:(600 * 0.9),
                                         SODMean = data[ , mean_sod],
                                         SODSDV = data[ , occ_sd],
                                         SODFreq = data[ , drink_freq],
                                         grams_ethanol_per_unit = grams_ethanol_per_unit)
    
    # might be able to avoid this step of converting to a list
    data[ , interval_prob_vec := lapply(seq_len(ncol(sod_probs)), function(i) sod_probs[ , i])]
    
  }
  
  # Set up store for alcohol risk trajectories
  
  if(!is.null(alc_indiv_risk_trajectories_store)) {
    
    # Update the stored relative risk file 
    # to retain only past relative risk trajectories for the individuals currently present in the simulation
    # this should help to minimise the memory usage
    alc_indiv_risk_trajectories_store <- alc_indiv_risk_trajectories_store[ran_id %fin% data[ , ran_id] & year > (k_year - 20)]
    
  }
  
  
  #########################################
  
  cat(paste0("\t\tCalculating risk for ", dn, " conditions\n"))
  
  ########
  # for alcohol - 
  # Note that these functions run slow for partially attributable acute conditions
  # one way to speed the process up is to avoid running the calculations more than once 
  # when the same result will be produced
  # this occurs for 
  # c("Assault", "Other_intentional_injuries") - which are both subject to the 'violence' risk function from Cherpitel 
  # c("Mechanical_forces", "Drowning", "Other_unintentional_injuries", "intentional_self_harm", "Accidental_poisoning", "Fire_injuries") - which all use the 'other' risk function
  # same is true for wholly attributable chronic and wholly attributable acute
  
  # Flag 0 or 1 if a risk function calculation for each of these groups has already occurred
  pa_violence_flag <- 0
  pa_other_flag <- 0
  wa_chronic_flag <- 0
  wa_acute_flag <- 0
  ########
  
  alc_indiv_risk_trajectories_temp <- NULL # 25-02-25 QA check
  
  # Loop through each disease
  for (i in 1:dn) {
    
    #i <- 5
    
    d <- as.character(diseases[i])
    
    if(isTRUE(show_progress)) {
      cat(paste0("\t\t\t", d, " ", round(100 * i / dn, 0), "%"))
    } else {
      cat(".")
    }
    
    #############################################################
    # Relative risks - alcohol
    
    if(d %in% alc_diseases_expanded & substance %in% c("alc", "tobalc")) {
      
      # d <- "Liver"
      
      # Setup names of temporary variables
      d_alc <- paste0(d, "_alcx")
      #d_alc_adj <- paste0(d, "_alc_adj")
      
      alc_mort_or_morb <- ifelse(stringr::str_detect(d, "_morb"), "morb", "mort")
      
      ##
      # Partial acute - violence
      if(d %in% c("Assault", "Other_intentional_injuries")) {
        
        if(pa_violence_flag == 0) {
          
          # Apply function that computes each individual's relative risk for a condition
          alcrr <- tobalcepi::RRalc(
            data = data,
            disease = d,
            mort_or_morb = alc_mort_or_morb,
            alc_protective = alc_protective,
            alc_wholly_chronic_thresholds = alc_wholly_chronic_thresholds,
            alc_wholly_acute_thresholds = alc_wholly_acute_thresholds,
            av_weekly_grams_per_day_var = "GPerDay",
            sex_var = "sex",
            age_var = "age",
            grams_ethanol_per_unit = grams_ethanol_per_unit,
            within_model = within_model
          )
          
          pa_violence_flag <- 1
          
          violence_rr <- alcrr
          
        }
        
        data[ , (d_alc) := violence_rr]
        
      }
      
      ##
      # Partial acute - other
      if(d %in% c("Mechanical_forces", "Drowning", "Other_unintentional_injuries", "intentional_self_harm", "Accidental_poisoning", "Fire_injuries")) {
        
        if(pa_other_flag == 0) {
          
          # Apply function that computes each individual's relative risk for a condition
          alcrr <- tobalcepi::RRalc(
            data = data,
            disease = d,
            mort_or_morb = alc_mort_or_morb,
            alc_protective = alc_protective,
            alc_wholly_chronic_thresholds = alc_wholly_chronic_thresholds,
            alc_wholly_acute_thresholds = alc_wholly_acute_thresholds,
            av_weekly_grams_per_day_var = "GPerDay",
            sex_var = "sex",
            age_var = "age",
            grams_ethanol_per_unit = grams_ethanol_per_unit,
            within_model = within_model
          )
          
          pa_other_flag <- 1
          
          other_rr <- alcrr
          
        }
        
        data[ , (d_alc) := other_rr]
        
      }
      
      ##
      # Wholly attributable chronic
      if(d %in% c(
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
      ) {
        
        if(wa_chronic_flag == 0) {
          
          # Apply function that computes each individual's relative risk for a condition
          alcrr <- tobalcepi::RRalc(
            data = data,
            disease = d,
            mort_or_morb = alc_mort_or_morb,
            alc_protective = alc_protective,
            alc_wholly_chronic_thresholds = alc_wholly_chronic_thresholds,
            alc_wholly_acute_thresholds = alc_wholly_acute_thresholds,
            av_weekly_grams_per_day_var = "GPerDay",
            sex_var = "sex",
            age_var = "age",
            grams_ethanol_per_unit = grams_ethanol_per_unit,
            within_model = within_model
          )
          
          wa_chronic_flag <- 1
          
          wa_chronic_ar <- alcrr
          
        }
        
        data[ , (d_alc) := wa_chronic_ar]
        
      }
      
      ##
      # Wholly attributable acute
      if(d %in% c(
        "Excessive_Blood_Level_of_Alcohol",
        "Toxic_effect_of_alcohol",
        "Alcohol_poisoning",
        "Evidence_of_alcohol_involvement_determined_by_blood_alcohol_level",
        "Acute_intoxication")
      ) {
        
        if(wa_acute_flag == 0) {
          
          # Apply function that computes each individual's relative risk for a condition
          alcrr <- tobalcepi::RRalc(
            data = data,
            disease = d,
            mort_or_morb = alc_mort_or_morb,
            alc_protective = alc_protective,
            alc_wholly_chronic_thresholds = alc_wholly_chronic_thresholds,
            alc_wholly_acute_thresholds = alc_wholly_acute_thresholds,
            av_weekly_grams_per_day_var = "GPerDay",
            sex_var = "sex",
            age_var = "age",
            grams_ethanol_per_unit = grams_ethanol_per_unit,
            within_model = within_model
          )
          
          wa_acute_flag <- 1
          
          wa_acute_ar <- alcrr
          
        }
        
        data[ , (d_alc) := wa_acute_ar]
        
      }
      
      ##
      # all the other alcohol related conditions
      # including cancers etc that will later have their relative risks lag adjusted
      if(!(d %in% c(
        "Assault", "Other_intentional_injuries",
        "Mechanical_forces", "Drowning", "Other_unintentional_injuries", "intentional_self_harm", "Accidental_poisoning", "Fire_injuries",
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
        "Mental_and_behavioural_disorders_due_to_use_of_alcohol",
        "Excessive_Blood_Level_of_Alcohol",
        "Toxic_effect_of_alcohol",
        "Alcohol_poisoning",
        "Evidence_of_alcohol_involvement_determined_by_blood_alcohol_level",
        "Acute_intoxication")
      )) {
        
        # Apply function that computes each individual's relative risk for a condition
        alcrr <- tobalcepi::RRalc(
          data = data,
          disease = d,
          mort_or_morb = alc_mort_or_morb,
          alc_protective = alc_protective,
          alc_wholly_chronic_thresholds = alc_wholly_chronic_thresholds,
          alc_wholly_acute_thresholds = alc_wholly_acute_thresholds,
          av_weekly_grams_per_day_var = "GPerDay",
          sex_var = "sex",
          age_var = "age",
          grams_ethanol_per_unit = grams_ethanol_per_unit,
          within_model = within_model
        )
        
        data[ , (d_alc) := alcrr]
        
      }
      
      
      if(isTRUE(alc_risk_lags) & d %in% alc_lag_diseases) {
        
        # Store the unadjusted relative risks
        # as the lagged diseases pass through this part of the code in the course of the loop through diseases,
        # the unadjusted relative risks for the lagged diseases will be stored in this 
        # year specific data table
        # this can then be joined onto the multi-year relative risk storage table at the end of this function
        if(is.null(alc_indiv_risk_trajectories_temp)) {
          
          alc_indiv_risk_trajectories_temp <- copy(data[ , c("ran_id", "year", "age", d_alc), with = F])
          
        } else {
          
          alc_indiv_risk_trajectories_temp <- merge(
            alc_indiv_risk_trajectories_temp,
            copy(data[ , c("ran_id", "year", d_alc), with = F]),
            by = c("ran_id", "year"), all = T, sort = F)
          
        }
      }
      
      #############################################################
      #############################################################
      
      # Extra method added to adapt to the new situation that STAPM tracks individuals
      # Keep this under review
      # as it differs from the SAPM method and might have an influence on results
      
      # also see code at the end of this function that initialises alc_indiv_risk_trajectories_store
      # if it is null in the first year of the simulation
      
      #alc_indiv_risk_trajectories_store <- copy(alc_indiv_risk_trajectories_temp)[ , year := 2016]
      
      
      if(isTRUE(alc_risk_lags) & !is.null(alc_indiv_risk_trajectories_store) & 
         
         d %in% alc_lag_diseases
         
      ) {
        
        # For the individuals present in the population sample for the current year,
        # add the relative risks for the current year
        # to the trajectories of past relative risks that have been stored for each individual
        
        indiv_risk_trajectories_alc <- rbindlist(list(
          
          # current alcohol risks for the individuals currently present in the simulation
          data[ , c("ran_id", "year", "age", d_alc), with = F], 
          
          # past relative risk trajectories for the individuals currently present in the simulation
          # for the focal disease
          alc_indiv_risk_trajectories_store[ , c("ran_id", "year", "age", d_alc), with = F]
          
        ), use.names = T)
        
        #indiv_risk_trajectories_alc_control <- fread("X:/HAR_PR/PR/ScotMUPupdate22/MUPmodelling/scottish-mup-uprating-v-2/30_outputs/alc_indiv_risk_trajectories_control_scotMUP_test_v5.txt")
        #indiv_risk_trajectories_alc <- fread("X:/HAR_PR/PR/ScotMUPupdate22/MUPmodelling/scottish-mup-uprating-v-2/30_outputs/alc_indiv_risk_trajectories_treatment_scotMUP_test_v5_1e.txt")
        
        #k_year <- 2040
        #d <- "Epilepsy"
        
        # New code added to back-fill the stored risk trajectories 
        # for the years within the 20 year risk history for each individual
        # for which they were not in the model
        
        # Make a new data structure that has a row for the past 20 years
        # for each individual currently in the model
        risk_domain <- setDT(data.frame(expand.grid(ran_id = unique(indiv_risk_trajectories_alc$ran_id),
                                                    year = (k_year - 19): k_year)))
        
        # Merge the above indiv_risk_trajectories_alc into this data structure
        # this will bring with it the stored relative risk and age
        indiv_risk_trajectories_alc <- merge(
          risk_domain, 
          indiv_risk_trajectories_alc, by = c("ran_id", "year"), all.x = T, all.y = F, sort = F)
        
        # Ages for individuals in years they were not in the model will be missing
        # fill the missing values for age by calculating the birth year for each individual
        indiv_risk_trajectories_alc[ , birth_year := unique(na.omit(year - age)), by = "ran_id"]
        indiv_risk_trajectories_alc[ , age := year - birth_year]
        
        testthat::expect_equal(nrow(indiv_risk_trajectories_alc[is.na(age)]), 0, info = "Missing values for age in alcohol risk trajectories")
        
        # Then do the back-filling of risk history
        
        # Fill missing RR values with the first stored value
        indiv_risk_trajectories_alc[ , min_year := min(year[!is.na(get(d_alc))]), by = "ran_id"]
        indiv_risk_trajectories_alc[ , first_rr := get(d_alc)[year == min_year[1]], by = "ran_id"]
        indiv_risk_trajectories_alc[year < min_year, (d_alc) := first_rr[1], by = "ran_id"]
        indiv_risk_trajectories_alc[ , `:=`(min_year = NULL, first_rr = NULL)]
        
        # Then apply the rule that age below 18 years always have RR = 1
        indiv_risk_trajectories_alc[age < 18, (d_alc) := 1]
        
        # Calculate the time differences to the current year
        #indiv_risk_trajectories_alc[ , years_since_change := year - k_year + 2] # 25-02-25 QA check [error]
        #indiv_risk_trajectories_alc[years_since_change > 20, years_since_change := 20] # 25-02-25 QA check
        indiv_risk_trajectories_alc[ , years_since_change := k_year - year + 1] # 25-02-25 QA check
        
        # Merge into the data the proportional reduction in relative risk
        # according to the time since alcohol consumption changed
        # Matching on the time difference to the current year
        indiv_risk_trajectories_alc <- merge(
          
          # the individual trajectories of relative risk
          indiv_risk_trajectories_alc,
          
          # the proportional reductions in relative risk
          tobalcepi::AlcLags(d), 
          
          by = "years_since_change", 
          
          all.x = T, all.y = F, sort = F)
        
        
        #test <- indiv_risk_trajectories_alc[ran_id == "52fb794fdf67ea7325f00ae0c8fc26c0", .(year, Epilepsy_alcx, years_since_change, prop_risk_reduction)]
        #test
        
        # Adjust the relative risk for the current year
        # to take into account the individual's past trajectory of relative risk
        # The adjusted relative risk for the current year is a weighted average of
        # the relative risks for all past years for which the individual was tracked
        # where the weights are the expected proportional reduction in risk
        
        # 25-02-25 QA check [error]        
        #indiv_risk_trajectories_alc_adjusted <- indiv_risk_trajectories_alc[ , 
        #list(rr_adj = sum(get(d_alc) * (1 + prop_risk_reduction), na.rm = T) / sum(1 + prop_risk_reduction, na.rm = T)), by = "ran_id"]
        
        # in the 25-02-25 QA check, the method of applying the lags to individual relative risks
        # was changed so that the weights were equal to the lag values from Holmes et al.
        # for cancers this means that for the first 10 years the weights are zero
        # but we cannot calculate a weighted average relative risks when all weights are zero
        
        # if it the case that all weights are zero, then 
        # either the model must have been running for 10 years or fewer
        # or that individual entered the model less than 10 years ago
        
        # and so the first relative risk value stored for each individual will be 
        # the relative risk assigned on entry to the model
        # that is assigned before any policy effect is applied
        # and so should be the same for each individual between the control and intervention arms
        
        # so in the case when all weights are zero, assign a weight of 1 to the first stored relative risk value
        # for that individual
        # this will keep assigning an individual their first stored value 
        # until there are two years with non-zero weight
        # at which point a new weighted relative risk will be assigned
        
        # determine the first year an individual had a stored RR value
        #indiv_risk_trajectories_alc[ , min_year := min(year), by = "ran_id"]
        
        # sum the weight values for that individual
        #indiv_risk_trajectories_alc[ , sum_prop := sum(prop_risk_reduction), by = "ran_id"]
        
        # if the sum of the weights is zero, then give the first year a weight of 1
        #indiv_risk_trajectories_alc[year == min_year & sum_prop == 0, prop_risk_reduction := 1]
        
        # if the sum of the weights is greater than zero but less than 1, 
        # then give the first year a weight of 1 - sum_prop
        #indiv_risk_trajectories_alc[year == min_year & sum_prop > 0 & sum_prop < 1, prop_risk_reduction := prop_risk_reduction + (1 - sum_prop)]
        
        # remove the variables that are now not needed
        #indiv_risk_trajectories_alc[ , `:=`(sum_prop = NULL, min_year = NULL)]
        
        # Calculate the lag-adjusted relative risk as a weighted average
        # of each individual's stored relative risk values
        
        testthat::expect_equal(nrow(indiv_risk_trajectories_alc[is.na(prop_risk_reduction)]), 0, info = "Missing values for prop_risk_reduction in alcohol risk trajectories")
        testthat::expect_equal(nrow(indiv_risk_trajectories_alc[is.na(get(d_alc))]), 0, info = paste0("Missing values for ", d_alc, " in alcohol risk trajectories"))
        
        indiv_risk_trajectories_alc_adjusted <- indiv_risk_trajectories_alc[ , list(rr_adj = sum(get(d_alc) * prop_risk_reduction, na.rm = T) / sum(prop_risk_reduction, na.rm = T)), by = "ran_id"]
        
        testthat::expect_equal(nrow(indiv_risk_trajectories_alc_adjusted[is.na(rr_adj)]), 0, info = "Missing values for adjusted rr in alcohol risk trajectories")
        
        # Remove the unadjusted relative risks from the data
        data[ , (d_alc) := NULL]
        
        # Assign the adjusted relative risk the appropriate disease-specific name
        setnames(indiv_risk_trajectories_alc_adjusted, "rr_adj", d_alc)
        
        # Merge the adjusted relative risks into the data
        data <- merge(
          data,
          indiv_risk_trajectories_alc_adjusted[ , c("ran_id", d_alc), with = F],
          by = "ran_id", all.x = T, all.y = F, sort = F)
        
      }
      
      
      #############################################################
      #############################################################
      
      
      # If the relative risk for alcohol does not need to feed forward
      # into a further calculation of joint relative risk for the disease being considered,
      # then the temporary name can be changed to be just the name of disease
      if(substance == "alc" | (substance == "tobalc" & !(d %in% intersect(alc_diseases_expanded, tob_diseases)))) {
        
        data[ , (d) := get(d_alc)]
        
      }
      
    }
    
    
    #############################################################
    # Relative risks - tobacco
    
    
    if(d %in% tob_diseases & substance %in% c("tob", "tobalc")) {
      
      # Setup names of temporary variables
      d_tob <- paste0(d, "_tob")
      d_tob_temp <- paste0(d, "_tob_temp")
      
      # Apply function that computes each individual's relative risk for a condition
      # Note - this applies the risk associated with current smoking to current and former smokers
      # to prepare for the later step in the calculation where the risk in former smokers
      # is adjusted to account for the decline in risk by time since quitting
      data[, (d_tob_temp) := tobalcepi::RRtob(
        data = data,
        disease = d # the name of the disease
      )]
      
      # After someone has been quit for 40 years, assume their risk is the same as a never smoker
      data[time_since_quit > 40, time_since_quit := 40]
      
      # Merge the proportional reduction in risk among former smokers into the data
      # Matching on the time since quit
      data <- merge(
        data,
        tobalcepi::TobLags(d, other_lag_function = other_lag_function),
        by = "time_since_quit", all.x = T, all.y = F, sort = F)
      
      data[is.na(prop_risk_reduction), prop_risk_reduction := 0L]
      
      # Calculate the relative risk for former smokers
      # by scaling the relative risk for current smokers for the change in risk expected
      # for each former smoker's number of years since quitting
      data[ , (d_tob) := (1 + (get(d_tob_temp) - 1) * (1 - prop_risk_reduction))]
      
      data[ , prop_risk_reduction := NULL]
      data[ , (d_tob_temp) := NULL]
      
      # If we don't want to consider the residual risks in former smokers,
      # then set the relative risks in former smokers to 1 i.e. the same as never smokers
      if(!isTRUE(tob_include_risk_in_former_smokers)) {
        
        data[smk.state == "former", (d_tob) := 1]
        
      }
      
      data[is.na(get(d_tob)), (d_tob) := 1]
      
      # If the relative risk for alcohol does not need to feed forward
      # into a further calculation of joint relative risk for the disease being considered,
      # then the temporary name can be changed to be just the name of disease
      if(substance == "tob" | (substance == "tobalc" & !(d %in% intersect(alc_diseases_expanded, tob_diseases)))) {
        
        setnames(data, d_tob, d)
        
      }
      
    }
    
    
    #############################################################
    # Relative risks - joint tobacco and alcohol risk
    
    
    if(d %in% intersect(alc_diseases, tob_diseases) & substance == "tobalc") {
      
      if(isTRUE(tobalc_include_int)) { # If synergy should be accounted for
        
        # Merge the appropriate synergy indexes
        # into the individual data
        # according to each individual's tobacco and alcohol consumption
        d_si <- paste0(d, "_si")
        
        data[ , (d_si) := tobalcepi::TobAlcInt(
          data = data,
          disease = d,
          alcohol_var = "weekmean",
          tobacco_var = "smk.state",
          rr.data = tobalc_int_data,
          account_for_synergy = TRUE
        )]
        
        # Combine the tobacco and alcohol relative risks
        # accounting for the Synergy Index - interaction of the additive scale
        data[ , (d) := 1 + (((get(d_alc) - 1) + (get(d_tob) - 1)) * get(d_si))]
        #data[ , (d) := get(d_alc) * get(d_tob) * get(d_si)] # Synergy Factor - multiplicative scale - NOT used - Prabhu Am J Gastroenterol 2014; 109:822–827; doi: 10.1038/ajg.2014.71
        
        data[ , (d_si) := NULL]
        
      } else { # If synergy should not be accounted for
        
        # Combine the tobacco and alcohol relative risks
        # *without* accounting for the synergy index
        data[ , (d) := 1 + ((get(d_alc) - 1) + (get(d_tob) - 1))] # additive scale
        #data[ , (d) := get(d_alc) * get(d_tob)] # multiplicative scale
        
      }
      
      #data[ , (d_alc) := NULL]
      data[ , (d_tob) := NULL]
      
    }
    
    
    kn <- NROW(data[get(d) < 0])
    testthat::expect_equal(kn, 0, info = paste0("RRFunc: Negative values in ", d, " after calculations"))
    
    
    if(isTRUE(show_progress)) {
      
      cat(crayon::green("\tdone\n"))
      
    } 
    
  }
  
  
  
  cat("\n")
  
  
  
  #############################################################
  # Store relative risks for alcohol for the current year
  
  if(stringr::str_detect(substance, "alc")) {
    
    if(isTRUE(alc_risk_lags)) {
      
      if(is.null(alc_indiv_risk_trajectories_store)) {
        
        # If the first year, then create the storage data table
        #alc_indiv_risk_trajectories_store <- data[ , c("ran_id", "year", paste0(alc_lag_diseases, "_alcx")), with = F] # 25-02-25 QA check
        alc_indiv_risk_trajectories_store <- copy(alc_indiv_risk_trajectories_temp)
        
        
      } else {
        
        # Otherwise append the relative risks for the current year to the stored data table
        alc_indiv_risk_trajectories_store <- rbindlist(list(
          alc_indiv_risk_trajectories_store,
          #data[ , c("ran_id", "year", paste0(alc_lag_diseases, "_alcx")), with = F] # 25-02-25 QA check
          copy(alc_indiv_risk_trajectories_temp)
        ), use.names = T)
        
      }
    }
    
    # Remove unadjusted alcohol relative risks for the current year
    data <- data[ , colnames(data)[sapply(colnames(data), function(x) !stringr::str_detect(x, "_alcx"))], with = F]
    
  }
  
  if(substance %in% c("alc", "tobalc")) {
    
    alc_vars <- c("drink_freq", "occ_sd", "mean_sod", "weight", "rwatson", "GPerDay", "interval_prob_vec")
    
    
    # Remove the variables that give alcohol consumption in grams
    data[ , (alc_vars) := NULL]
    
    
  }
  
  
  
  # Outputs
  
  if(is.null(alc_indiv_risk_trajectories_store)) {
    
    return(data)
    
  } else {
    
    return(list(
      data_plus_rr = data,
      new_alc_indiv_risk_trajectories_store = alc_indiv_risk_trajectories_store
    ))
  }
}





