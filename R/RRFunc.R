
#' Individual relative risks of disease
#'
#' This function takes a sample of individuals and computes each individual's relative risk
#' for each disease according to their current tobacco and alcohol consumption. There is an option to tailor this
#' to the alcohol only, tobacco only, or joint tobacco and alcohol contexts.
#'
#' ALCOHOL
#'
#' For alcohol, the relative risk for each individual for each disease is calculated based on their average weekly alcohol consumption.
#'  For diseases that have separate mortality and morbidity risk functions, separate variables are created containing
#'  the relative risks for each for the same disease.
#' Individuals are not recorded as being former drinkers -- instead their alcohol consumption just falls to zero and their
#' relative risk for disease changes accordingly.
#'
#' Alcohol lags:
#'
#' To account for the lagged effects of individual drinking history on their
#' current risk of disease, we add memory by storing each individual's past trajectory of their relative risk for each disease.
#' In the model, the current relative risk is then adjusted to take account of each individual's stored drinking histories -
#' this adjustment takes the form of a weighted average of current and past relative risk where the weights are proportional to
#' the disease specific lag function that describes the gradual emergence of an effect of changed consumption on risk over time.
#' This uses a slightly different method to SAPM.
#'
#' TOBACCO
#'
#' For tobacco, the relative risk for each individual is calculated based on whether they are a current, former or never smoker.
#' Currently, all current smokers have the same relative risk regardless of the amount they currently smoke or have smoked in the past.
#'
#' Tobacco lags:
#'
#' Former smokers are initially given the relative risk associated with current smokers, which we then scale according to a disease-specific
#' function that describes how risk declines after quitting smoking.
#'
#' ALCOHOL AND TOBACCO
#'
#' If both tobacco and alcohol are being considered in a joint model,
#' we combine the relative risks for current drinkers and smokers. For oral, pharyngeal, laryngeal and oesophageal cancers we also
#' have the option of scaling the joint risks by a 'synergy index', which takes the result of a meta-analysis of the additional
#' risk faced by people because they consume both tobacco and alcohol.
#'
#' @param data Data table of individual characteristics - this function uses current smoking and drinking status/amount.
#' @param substance Whether to compute relative risks for just alcohol ("alc"),
#' just tobacco ("tob") or joint risks for tobacco and alcohol ("tobalc").
#' @param k_year Integer giving the current year of the simulation.
#' @param alc_diseases Character vector of alcohol related diseases.
#' @param alc_mort_and_morb Character vector of alcohol related diseases that have separate risk functions for
#' mortality and morbidity.
#' @param alc_risk_lags Logical - should each individual's relative risks for alcohol be lagged according to
#' their past trajectory of relative risks. Defaults to FALSE. This should only be set to TRUE for a model run that simulates individual trajctories,
#' and should be FALSE if used as part of the current method for calculating attributable fractions.
#' @param alc_indiv_risk_trajectories_store Data table that stores the individual history of relative risks for alcohol related diseases.
#' @param alc_protective Logical - whether to include the protective effects of
#' alcohol in the risk function. Defaults to TRUE. If TRUE, then the part of the risk function < 1 is set to equal 1.
#' @param alc_wholly_chronic_thresholds Numeric vector - the thresholds in units/week over
#'  which individuals begin to experience an elevated risk
#'  for chronic diseases that are wholly attributable to alcohol. Input in the form c(male, female).
#' @param alc_wholly_acute_thresholds Numeric vector - the thresholds in units/day over
#'  which individuals begin to experience an elevated risk
#'  for acute diseases that are wholly attributable to alcohol. Input in the form c(male, female).
#' @param grams_ethanol_per_unit Numeric value giving the conversion factor for the number of grams of pure
#' ethanol in one UK standard unit of alcohol.
#' @param tob_diseases Character vector of tobacco related diseases.
#' @param tob_include_risk_in_former_smokers Logical - whether the residual risks of smoking in former smokers
#' should be considered (defaults to TRUE).
#' @param tobalc_include_int Logical - in computing joint relative risks for tobacco and alcohol,
#'  should a (synergystic/multiplicative) interaction between exposure to tobacco and alcohol be included.
#'  Defaults to FALSE. If TRUE, then only interactive effects for oesophageal, pharynx, oral cavity and larynx cancers
#'  are considered.
#' @param tobalc_int_data Data table containing the disease-specific interactions between tobacco and alcohol.
#' @param show_progress Logical - Should the progress of the loop through diseases be shown. Defaults to FALSE.
#'
#' @return Two data tables are returned:
#' \itemize{
#' \item "data_plus_rr" is a copy of "data" with added columns that give each
#' individual's relative risk for each disease.
#' \item "new_alc_indiv_risk_trajectories_store" is a copy of "alc_indiv_risk_trajectories_store" with
#' the relative risks for the current year added to the store.
#' }
#' @export
#'
#' @examples
#'
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
#'   alc_wholly_acute_thresholds = c(3, 3),
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
#'   alc_wholly_acute_thresholds = c(3, 3),
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
#'   show_progress = TRUE
#' )
#'
#'
#' #############################
#' ## TOBACCO AND ALCOHOL
#'
#'
RRFunc <- function(
  data,
  substance = c("tob", "alc", "tobalc"),
  k_year = NULL,
  alc_diseases = c("Pharynx", "Oral_cavity"),
  alc_mort_and_morb = c("Ischaemic_heart_disease", "LiverCirrhosis"),
  alc_risk_lags = TRUE,
  alc_indiv_risk_trajectories_store = NULL,
  alc_protective = TRUE,
  alc_wholly_chronic_thresholds = c(6, 8),
  alc_wholly_acute_thresholds = c(6, 8),
  grams_ethanol_per_unit = 8,
  tob_diseases = c("Pharynx", "Oral_cavity"),
  tob_include_risk_in_former_smokers = TRUE,
  tobalc_include_int = FALSE,
  tobalc_int_data = NULL,
  show_progress = FALSE
) {


  data <- copy(data)

  # Organise disease lists

  if(substance == "alc") {
    # For the diseases that have separate risk functions for mortality and morbidity
    # expand the list of diseases so that the mortality and morbidity versions
    # are included as separate variables

    # Set the default as mortality
    # and mark the additions to the disease list with the postscript "_morb"
    alc_diseases <- c(alc_diseases, paste0(alc_mort_and_morb, "_morb"))
    diseases <- alc_diseases
  }
  if(substance == "tob") {
    diseases <- tob_diseases
  }
  if(substance == "tobalc") {
    diseases <- union(alc_diseases, tob_diseases)
    mort_and_morb_diseases <- union(alc_mort_and_morb, diseases)
    diseases <- c(diseases, paste0(mort_and_morb_diseases, "_morb"))
  }

  dn <- length(diseases)

  message(paste0("\t\tCalculating risk for ", dn, " conditions"))

  for (i in 1:dn) {

    d <- as.character(diseases[i])

    if(isTRUE(show_progress)) message(paste0("\t\t\t", d, " ", round(100 * i / dn, 0), "%"))

    #############################################################
    # Relative risks - alcohol

    if(d %in% alc_diseases & substance %in% c("alc", "tobalc")) {

      # Calculate the parameters of the binge model - based on average weekly consumption
      data <- tobalcepi::AlcBinge(data)

      # Convert units to grams of alcohol / truncate
      data[ , GPerDay := weekmean * (grams_ethanol_per_unit / 7)]
      data[GPerDay >= 150, GPerDay := 150]
      data[ , peakday_grams := peakday * grams_ethanol_per_unit]

      # Setup names of temporary variables
      d_alc <- paste0(d, "_alc")
      d_alc_adj <- paste0(d, "_alc_adj")

      alc_mort_or_morb <- ifelse(stringr::str_detect(d, "_morb"), "morb", "mort")

      # Apply function that computes each individual's relative risk for a condition
      data[ , (d_alc) := tobalcepi::RRalc(
        data = data,
        disease = d,
        mort_or_morb = alc_mort_or_morb,
        protective = alc_protective,
        alc_wholly_chronic_thresholds = alc_wholly_acute_thresholds * grams_ethanol_per_unit,
        alc_wholly_acute_thresholds = alc_wholly_acute_thresholds * grams_ethanol_per_unit
      )]

      # Remove the variables that give alcohol consumption in grams
      data[ , `:=`(GPerDay = NULL, peakday_grams = NULL)]

      if(isTRUE(alc_risk_lags) & !is.null(alc_indiv_risk_trajectories_store)) {

        # For the individuals present in the population sample for the current year,
        # add the relative risks for the current year
        # to the trajectories of past relative risks that have been stored for each individual
        indiv_risk_trajectories_alc <- rbindlist(list(
          data[ , c("ran_id", "year", d_alc), with = F], # current alcohol risks
          alc_indiv_risk_trajectories_store[ran_id %in% data[ , ran_id], c("ran_id", "year", d_alc), with = F] # past relative risk trajectories
        ), use.names = T)

        # Calculate the time differences to the current year
        indiv_risk_trajectories_alc[ , years_since_change := year - k_year + 2]
        indiv_risk_trajectories_alc[years_since_change > 20, years_since_change := 20]

        # Merge into the data the proportional reduction in relative risk
        # according to the time since alcohol consumption changed
        # Matching on the time difference to the current year
        indiv_risk_trajectories_alc <- merge(
          indiv_risk_trajectories_alc, # the individual trajectories of relative risk
          tobalcepi::AlcLags(d), # the proportional reductions in relative risk
          by = c("years_since_change"), all.x = T, all.y = F, sort = F)

        # Adjust the relative risk for the current year
        # to take into account the individual's past trajectory of relative risk
        # The adjusted relative risk for the current year is a weighted average of
        # the relative risks for all past years for which the individual was tracked
        # where the weights are the expected proportional reduction in risk
        # which means that the relative risk for the current year always has the lowest weight
        # reflecting the lagged link between current consumption and relative risk
        indiv_risk_trajectories_alc_adjusted <- indiv_risk_trajectories_alc[ ,
          .(rr_adj = sum(get(d_alc) * (1 + prop_risk_reduction), na.rm = T) / sum(1 + prop_risk_reduction, na.rm = T)),
          by = "ran_id"]

        # Remove the unadjusted relative risks from the data
        data[ , (d_alc) := NULL]

        # Assign the adjusted relative risk the appropriate disease-specific name
        setnames(indiv_risk_trajectories_alc_adjusted, "rr_adj", d_alc)

        # Marge the adjused relative risks into the data
        data <- merge(
          data,
          indiv_risk_trajectories_alc_adjusted[ , c("ran_id", d_alc), with = F],
          by = "ran_id", sort = F)

      }

      # If the relative risk for alcohol does not need to feed forward
      # into a further calculation of joint relative risk for the disease being considered,
      # then the temporary name can be changed to be just the name of disease
      if(substance == "alc" | (substance == "tobalc" & !(d %in% intersect(alc_diseases, tob_diseases)))) {
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
        tobalcepi::TobLags(d),
        by = c("time_since_quit"), all.x = T, all.y = F, sort = F)

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
      if(substance == "tob" | (substance == "tobalc" & !(d %in% intersect(alc_diseases, tob_diseases)))) {
        setnames(data, d_tob, d)
      }

    }

    #############################################################
    # Relative risks - tobacco and alcohol

    if(d %in% intersect(alc_diseases, tob_diseases) & substance == "tobalc") {

      if(isTRUE(tobalc_include_int)) {

        # Synergy index
        d_si <- paste0(d, "_si")

        data[ , (d_si) := tobalcepi::TobAlcInt(
          condition_TobAlcInt = d,
          cons_alc_TobAlcInt = "weekmean",
          cons_tob_TobAlcInt = "smk.state",
          rr.data_TobAlcInt = tobalc_int_data,
          data_TobAlcInt = data
        )]

        data[ , (d) := (1 + ((get(d_alc) - 1) + (get(d_tob) - 1)) * get(d_si))]

        data[ , (d_si) := NULL]

      } else {

        data[ , (d) := 1 + ((get(d_alc) - 1) + (get(d_tob) - 1))]

      }

      #data[ , (d_alc) := NULL]
      data[ , (d_tob) := NULL]

    }

    if(isTRUE(show_progress)) message("\t\t\t\tdone")

  }



  #############################################################
  # Store relative risks for alcohol for the current year

  if(stringr::str_detect(substance, "alc")) {

    if(isTRUE(alc_risk_lags)) {

      if(is.null(alc_indiv_risk_trajectories_store)) {

        # If the first year, then create the storage data table
        alc_indiv_risk_trajectories_store <- copy(data[ , c("ran_id", "year", paste0(alc_diseases, "_alc")), with = F])

      } else {

        # Otherwise append the relative risks for the current year to the stored data table
        alc_indiv_risk_trajectories_store <- rbindlist(list(
          alc_indiv_risk_trajectories_store,
          copy(data[ , c("ran_id", "year", paste0(alc_diseases, "_alc")), with = F])
        ), use.names = T)

      }
    }

    # After storing, remove unadjusted alcohol relative risks for the current year
    data <- data[ , colnames(data)[sapply(colnames(data), function(x) !stringr::str_detect(x, "_alc"))], with = F]

  }


  # Outputs

  if(is.null(alc_indiv_risk_trajectories_store)) {
    return(copy(data))
  } else {

    return(list(
      data_plus_rr = copy(data),
      new_alc_indiv_risk_trajectories_store = alc_indiv_risk_trajectories_store
    ))
  }
}















































