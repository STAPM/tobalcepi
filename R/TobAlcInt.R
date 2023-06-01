

#' Risk interaction between tobacco and alcohol
#'
#' Assigns the disease-specific interaction term (synergy index) appropriate to each
#' individual's tobacco and alcohol consumption.
#'
#' We currently include estimates of synergistic effects for 
#' oral, pharyngeal, laryngeal and oesophageal cancers 
#' (we describe these data and their sources in our tobacco risk functions report 
#' \insertCite{webster2018risk;textual}{tobalcepi}). 
#' The data sources we use are \insertCite{Prabhu2014;textual}{tobalcepi} 
#'  and \insertCite{Hashibe2009;textual}{tobalcepi}.
#' We apply these effects by scaling the joint risks by a 'synergy index', 
#' which takes the result of a meta-analysis of the additional
#' risk faced by people because they consume both tobacco and alcohol.   
#'
#' @param data Data table - containing the individual characteristics of smokers and drinkers 
#' @param disease Character
#' @param alcohol_var Character
#' @param tobacco_var Character
#' @param rr.data Data table
#' @param account_for_synergy Logical
#'
#' @return Returns a numeric vector containing  of each individual's relative risks for the tobacco-related disease
#' specified by "disease".
#' 
#' @importFrom data.table := setDT setnames copy
#' 
#' @export
#' @references
#' \insertRef{webster2018risk}{tobalcepi}  
#' 
#' \insertRef{Hashibe2009}{tobalcepi}  
#' 
#' \insertRef{Prabhu2014}{tobalcepi}  
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' TobAlcInt()
#' 
#' }
#' 
#' 
TobAlcInt <- function(
  data,
  disease = "Pharynx",
  alcohol_var = "weekmean",
  tobacco_var = "smk.state",
  rr.data,
  account_for_synergy = TRUE
) {

  rr <- rr.data[Disease == disease]

  dtmp <- copy(data[ , list(x.tob = get(tobacco_var), x.alc = get(alcohol_var))])

  # Conditions with a tobacco alcohol interaction
  str1 <- c(
    "Oral_cavity",
    "Pharynx",
    "Larynx",
    "Oesophageal_SCC"
  )

  # Calculate the synergy index.
  if(disease %fin% str1) {

    alc1_tob0 <- rr[ , alc1_tob0]
    alc0_tob1 <- rr[ , alc0_tob1]
    alc1_tob1 <- rr[ , alc1_tob1]

    #si <- (alc1_tob1 - 1) / ((alc1_tob0 - 1) + (alc0_tob1 - 1))
    si <- alc1_tob1 / (alc1_tob0 + alc0_tob1)

  } else {

    si <- 1

  }

  # Calculate when to apply the synergy index to an individual.
  dtmp[ , si.indiv := ifelse(x.tob == "current" & x.alc > 0, si, 1)]


# Final output
return(dtmp[ , si.indiv])
}



