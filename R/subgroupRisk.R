
#' Summarise relative risk
#'
#' Calculate the sum of the relative risk for all individuals in a subgroup,
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
#'
#' @return Returns a data table containing the subgroup specific summaries for each disease.
#' @importFrom data.table := setDT setnames
#' @export
#' 
#' \lifecycle{stable}
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
  subgroups = c("sex", "age_cat")
) {

  out <- data.table::copy(data)

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

  # Standardise the relative risks by subtracting 1 and multiplying by the weight
  for (d in disease_names) {
    out[, (paste0(d, "_z")) := weight * (get(d) - 1)]
  }

  ############################################################
  # To prepare for subsequent computation of a PIF
  # compute the average relative risk within a subgroup

  # compute the average rather than the total, so that when we later calculate the ratio
  # of this aggregated relative risk between treatment and control arms,
  # the ratio is not influenced by differences in the number of individuals
  # i.e. we want to calculate the ratio of the expected value of individual risk in each arm

  if(!isTRUE(af)) {

    # calculate average relative risk
    out_risk <- out[,
                    lapply(.SD, function(x) {
                      sum(x, na.rm = T)
                    }),
                    by =  c(subgroups, "year"),
                    .SDcols = paste0(disease_names, "_z")]

    data.table::setnames(out_risk, paste0(disease_names, "_z"), disease_names)

    out_risk <- data.table::melt(
      out_risk,
      id.vars = c(subgroups, "year"),
      variable.name = "condition",
      value.name = paste0("av_risk_", label)
    )

  }

  ############################################################
  # For attributable fractions

  if(isTRUE(af)) {

    # calculate attributable fractions, considering residual risk in former smokers
    out_risk <- out[,
                  lapply(.SD, function(x) {
                    sum(x, na.rm = T) / (sum(x, na.rm = T) + 1)
                  }),
                  by =  c(subgroups, "year"),
                  .SDcols = paste0(disease_names, "_z")]

    data.table::setnames(out_risk, paste0(disease_names, "_z"), disease_names)

    out_risk <- data.table::melt(
      out_risk,
      id.vars = c(subgroups, "year"),
      variable.name = "condition",
      value.name = "af"
    )

    # Set the AAF = 1 for wholly attributable conditions
    out_risk[condition %in% c(
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
      "Mental_and_behavioural_disorders_due_to_use_of_alcohol"
    ), af := 1]

  }

return(out_risk)
}


