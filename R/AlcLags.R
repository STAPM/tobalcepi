
#' Alcohol lag times
#'
#' Prepare the disease specific functions that describe how a change in alcohol consumption
#' gradually has an effect on the relative risk of disease incidence over time (up to 20 years)
#' since alcohol consumption changed.
#'
#' All lag times are taken from the review by \insertCite{holmes2012temporal;textual}{tobalcepi} ,
#'  and are the numbers used in the current version of SAPM.
#'
#' @param disease_name Character - the name of the disease under consideration.
#' @param n_years Integer - the number of years from 1 to n over which the effect of a change in
#' consumption emerges. Defaults to 20 years to fit with the current lag data.
#'
#' @return Returns a data table with two columns - one for the years since consumption changed, and the other
#' that gives the proportion by which the effect of a change in consumption
#' on an individual's relative risk of disease has so far emerged.
#' 
#' @importFrom data.table := setDT setnames data.table
#' @importFrom stapmr %fin%
#' 
#' @references
#' \insertRef{holmes2012temporal}{tobalcepi}  
#' 
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' AlcLags("Pharynx")
#'}
AlcLags <- function(
  disease_name = c("Pharynx", "Oral_cavity"),
  n_years = 20
) {

  #################################
  # List the specific diseases that fall under each functional form of lag time

  cancer_lags <- c("Pharynx", "Oral_cavity", "Oesophageal_SCC", "Colorectal", "Liver",
    "Larynx", "Pancreas", "Breast")

  alc_specific_lags <- c("Alcohol_induced_pseudoCushings_syndrome", "Degeneration", "Alcoholic_polyneuropathy",
    "Alcoholic_myopathy", "Alcoholic_cardiomyopathy")

  maternal_care_lag <- "Maternal_care_for_suspected_damage_to_foetus_from_alcohol"

  digestive_lags <- c("LiverCirrhosis", "Chronic_Pancreatitis", "Acute_Pancreatitis",
    "Acute_pancreatitis_alcohol_induced", "Chronic_pancreatitis_alcohol_induced")

  alc_liver_disease <- "Alcoholic_liver_disease"

  diabetes_lags <- c("Diabetes", "HypertensiveHeartDisease", "Cardiac_Arrhythmias")

  cvd_lags <- c("Ischaemic_heart_disease", "Haemorrhagic_Stroke", "Ischaemic_Stroke")

  epilepsy_lag <- "Epilepsy"

  alcoholic_gastritis_lag <- "Alcoholic_gastritis"

  respiratory_lags <- c("Tuberculosis", "Influenza_clinically_diagnosed",
    "Influenza_microbiologically_confirmed", "Pneumonia")

  #################################
  # Specify the functional forms of the lags
  # The numbers are taken from SAPM - Holmes et al. 2012

  # Set the default as an instant reduction of risk e.g. for acute conditions
  lag_func <- c(100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  if(disease_name %fin% cancer_lags) {
    lag_func <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  }

  if(disease_name %fin% alc_specific_lags) {
    lag_func <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
  }

  if(disease_name %fin% maternal_care_lag) {
    lag_func <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  if(disease_name %fin% digestive_lags) {
    lag_func <- c(20.2333, 16.1866, 12.9493, 10.3594, 8.2875, 6.6300, 5.3040, 4.2432, 3.3946, 2.7157, 2.1725, 1.7380, 1.3904, 1.1123, 0.8899, 0.7119, 0.5695, 0.4556, 0.3645, 0.2916)
  }

  if(disease_name %fin% alc_liver_disease) {
    lag_func <- c(20.6721, 13.1575, 9.2027, 7.0416, 5.7902, 5.0057, 4.4657, 4.0583, 3.7268, 3.4422, 3.1894, 2.9602, 2.7500, 2.5561, 2.3764, 2.2097, 2.0548, 1.9109, 1.7771, 1.6527)
  }

  if(disease_name %fin% diabetes_lags) {
    lag_func <- c(22.4058, 17.9246, 14.3397, 11.4718, 9.1774, 7.3419, 5.8735, 4.6988, 3.7591, 3.0073, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  if(disease_name %fin% cvd_lags) {
    lag_func <- c(30.8721, 21.6104, 15.1273, 10.5891, 7.4124, 5.1887, 3.6321, 2.5424, 1.7797, 1.2458, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  if(disease_name %fin% epilepsy_lag) {
    lag_func <- c(43.3727, 26.0236, 15.6142, 9.3685, 5.6211, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  if(disease_name %fin% alcoholic_gastritis_lag) {
    lag_func <- c(50.0489, 25.0244, 12.5122, 6.2561, 3.1281, 1.5640, 0.7820, 0.3910, 0.1955, 0.0978, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  if(disease_name %fin% respiratory_lags) {
    lag_func <- c(60.6208, 24.2483, 9.6993, 3.8797, 1.5519, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  #################################
  # Format the output

  # The numbers above are currently in the form of a percentage change per year
  # Re-format so they show the cumulative proportion by which risk reduces over time
  # i.e. after 20 years, all excess risk has gone, so the cumulative proportion of risk reduction = 1

  lag_data <- data.table(
    years_since_change = 1:n_years,
    prop_risk_reduction = cumsum(lag_func) / 100
  )
  

return(lag_data)
}

























