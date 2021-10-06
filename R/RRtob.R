
#' Tobacco relative risks
#'
#' Relative risks for current vs. never cigarette smokers.
#'
#' We focus on the risks of current smoking and limit ourselves to diseases that affect the consumer themselves e.g.
#'  excluding secondary effects of smoking on children.
#'  We assume the equivalence of relative risks and odds ratios.
#'  Our starting point was the Royal College of Physician's (RCP) report "Hiding in plain sight:
#'  Treating tobacco dependency in the NHS",
#'  which reviewed smoking-disease associations to produce an updated list of diseases that are caused
#'  by smoking and updated risk sources.
#'  We mainly keep to the RCP report's disease list and risk functions, with any deviations from the RCP list
#'   and risk sources being for one of two reasons:
#'   \itemize{
#'   \item{There are often slightly conflicting ICD-10 code definitions used for some diseases and
#'    we have sought to harmonise these consistently across both tobacco and alcohol,
#'     based on the Sheffield Alcohol Policy Model (SAPM) v4.0 disease list;}
#'     \item{Since publication of the RCP report, Cancer Research UK (CRUK) produced their own disease
#'     list and risk sources for cancers attributable to modifiable risk factors,
#'     including tobacco and alcohol.
#'     Discussions with CRUK shaped the disease definitions in our updated Sheffield disease list for alcohol.
#'     Where there are differences in the risk sources used in the RCP report and CRUK's work,
#'     we take the estimate that matches most closely to our disease definitions, or the more recent estimate.}
#'  }
#'
#' @param data Data table of individual characteristics.
#' @param disease Character - the name of the disease for which the relative risks will be computed.
#' @param smoker_status_var Character - the name of the variable containing whether an individual is
#'  a current, former or never smoker.
#' @param sex_var Character - the name of the variable containing individual sex.
#' @param age_var Character - the name of the variable containing individual age in single years.
#' @param rr_data Data table containing the relative risks of current vs. never smokers.
#' The data table "tobacco_relative_risks" is embedded within the stapmr package.
#'
#' @return Returns a numeric vector of each individual's relative risks for the tobacco-related disease
#' specified by "disease".
#' 
#' @importFrom data.table := setDT setnames copy
#' @importFrom stapmr %fin%
#' 
#' @export
#' 
#'
#' @examples
#'\dontrun{
#' # Example data
#'
#' n <- 1e2
#'
#' data <- data.table(
#'   smk.state = sample(x = c("current", "former", "never"), size = n, replace = T),
#'   sex = "Female",
#'   age = 30
#' )
#'
#' # Apply the function
#' test <- RRtob(
#'   data,
#'   disease = "Pharynx"
#' )
#'}
RRtob <- function(
  data,
  disease = "Pharynx",
  smoker_status_var = "smk.state",
  sex_var = "sex",
  age_var = "age",
  rr_data = tobalcepi::tobacco_relative_risks
) {

  # Check that zero risk is specified as 1
  rr_data[relative_risk == 0, relative_risk := 1]

  # Select the relative risks for the focal disease
  rr <- rr_data[condition == disease]

  # Select the consumption, age and sex columns
  data_temp <- copy(data[ , .(x = get(smoker_status_var), sex = get(sex_var), age = get(age_var))])

  data_temp[ , ageband := c(
    "<35",
    "35-44",
    "45-54",
    "55-64",
    "65-74",
    "75+")[findInterval(age, c(-1, 35, 45, 55, 65, 75))]]

  # As default return 1.
  data_temp[ , rr_indiv := 1]

  #######################################
  # Diseases where risk differs by sex and age (35-64, 65+)
  str3 <- c("Ischaemic_heart_disease")

  if(disease %fin% str3) {

    # Assign risks for current smokers

    data_temp[x == "current" & ageband %fin% c("<35", "35-44", "45-54", "55-64") & sex == "Male",
      rr_indiv := rr[sex == "Male" & age == "35-64", relative_risk]]

    data_temp[x == "current" & ageband %fin% c("65-74", "75+") & sex == "Male",
      rr_indiv := rr[sex == "Male" & age == "65+", relative_risk]]

    data_temp[x == "current" & ageband %fin% c("<35", "35-44", "45-54", "55-64") & sex == "Female",
      rr_indiv := rr[sex == "Female" & age == "35-64", relative_risk]]

    data_temp[x == "current" & ageband %fin% c("65-74", "75+") & sex == "Female",
      rr_indiv := rr[sex == "Female" & age == "65+", relative_risk]]

    # Also assign the risks for current smoking to former smokers
    # as the risk for former smokers will subsequently be reduced according to the number of years
    # since these former smokers quit

    data_temp[x == "former" & ageband %fin% c("<35", "35-44", "45-54", "55-64") & sex == "Male",
                    rr_indiv := rr[sex == "Male" & age == "35-64", relative_risk]]

    data_temp[x == "former" & ageband %fin% c("65-74", "75+") & sex == "Male",
                    rr_indiv := rr[sex == "Male" & age == "65+", relative_risk]]

    data_temp[x == "former" & ageband %fin% c("<35", "35-44", "45-54", "55-64") & sex == "Female",
                    rr_indiv := rr[sex == "Female" & age == "35-64", relative_risk]]

    data_temp[x == "former" & ageband %fin% c("65-74", "75+") & sex == "Female",
                    rr_indiv := rr[sex == "Female" & age == "65+", relative_risk]]

  }

  #######################################
  # Diseases where risk differs by sex only
  str4 <- c(

    "Haemorrhagic_Stroke",
    "Ischaemic_Stroke",

    "Oral_cavity",
    "Pharynx",
    "Lung",
    "Nasopharynx_sinonasal",
    "Larynx",
    "Oesophageal_AC",
    "Oesophageal_SCC",
    "Stomach",
    "Pancreas",
    "Liver",
    "Colorectal",
    "Kidney",
    "Lower_urinary_tract",
    "Bladder",
    "Cervical",
    "Acute_myeloid_leukaemia",

    "Peripheral_arterial_disease",
    "Abdominal_aortic_aneurysm",
    "Venous_thromboembolism",
    "Chronic_obstructive_pulmonary_disease",
    "Asthma",
    "Tuberculosis",
    "Obstructive_sleep_apnoea",
    "Pneumonia",
    "Influenza_clinically_diagnosed",
    "Influenza_microbiologically_confirmed",
    "Diabetes",
    "Alzheimers_disease",
    "Vascular_dementia",
    "All_cause_dementia",
    "Depression",
    "Schizophrenia",
    "Multiple_sclerosis",
    "Systematic_lupus_erythematosis",
    "Low_back_pain",
    "Psoriasis",
    "Age_related_macular_degeneration",
    "Crohns_disease",
    "Hip_fracture",
    "Idiopathic_pulmonary_fibrosis",
    "Rheumatoid_arthritis",
    "Chronic_Kidney_disease",
    "End_stage_renal_disease",
    "Senile_cataract",
    "Bulimia",
    "Hearing_loss",
    "Psychosis",

    "Ulcerative_colitis",
    "Parkinson"
  )

  if(disease %fin% str4) {

    # Assign risks for current smokers

    data_temp[x == "current" & sex == "Female", rr_indiv := rr[sex == "Female", relative_risk]]
    data_temp[x == "current" & sex == "Male", rr_indiv := rr[sex == "Male", relative_risk]]

    # Also assign the risks for current smoking to former smokers
    # as the risk for former smokers will subsequently be reduced according to the number of years
    # since these former smokers quit

    data_temp[x == "former" & sex == "Female", rr_indiv := rr[sex == "Female", relative_risk]]
    data_temp[x == "former" & sex == "Male", rr_indiv := rr[sex == "Male", relative_risk]]

  }


# Output a vector containing the relative risks for each individual
return(data_temp[ , rr_indiv])
}



