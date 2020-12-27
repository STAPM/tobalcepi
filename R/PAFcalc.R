
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
#' @param use_weights Logical - should the calculation account for survey weights. Defaults to FALSE.
#' Weight variable must be called "wt_int".
#' @param year_range Either an integer vector of the years to be selected or "all". Defaults to "all".
#' @param pool Logical - should the years selected be pooled. Defaults to FALSE.
#' @param subgroups Character vector - the variable names of the subgroups used to stratify the estimates.
#' 
#' @return Returns a data.table containing the estimated PAFs.
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
  use_weights = FALSE,
  year_range = "all",
  pool = FALSE,
  subgroups = c("sex", "age_cat")
) {
  
  # Add the relative risks to the data
  data_rr <- tobalcepi::RRFunc(
    data = data,
    substance = substance,
    tob_diseases = tobalcepi::tob_disease_names,
    tob_include_risk_in_former_smokers = tob_include_risk_in_former_smokers,
    show_progress = FALSE)
  
  # Calculate PAFs
  paf_data <- subgroupRisk(
    data = data_rr,
    af = TRUE,
    disease_names = tobalcepi::tob_disease_names,
    use_weights = use_weights,
    pool = pool,
    subgroups = subgroups)
  
  
  return(paf_data[])
}



