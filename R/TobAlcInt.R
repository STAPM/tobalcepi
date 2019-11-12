

#' Risk interaction between tobacco and alcohol
#'
#' Assigns the disease-specific interaction term (synergy index) appropriate to each
#' individual's tobacco and alcohol consumption.
#'
#'
#'
#'
#' @param data Data table
#' @param disease Character
#' @param alcohol_var Character
#' @param tobacco_var Character
#' @param rr_data Data table
#' @param account_for_synergy Logical
#'
#' @return Returns a numeric vector containing  of each individual's relative risks for the tobacco-related disease
#' specified by "disease".
#' @export
#'
#' @examples
TobAlcInt <- function(
  data,
  disease = "Pharynx",
  alcohol_var = "weekmean",
  tobacco_var = "smk.state",
  rr_data,
  account_for_synergy = TRUE
) {

  rr <- rr.data[Disease == disease]

  dtmp <- copy(data[ , .(x.tob = get(tobacco_var), x.alc = get(alcohol_var))])

  # Conditions with a tobacco alcohol interaction
  str1 <- c(
    "Oral_cavity",
    "Pharynx",
    "Larynx",
    "Oesophageal_SCC"
  )

  # Calculate the synergy index.
  if(disease %in% str1) {

    alc1_tob0 <- rr[ , alc1_tob0]
    alc0_tob1 <- rr[ , alc0_tob1]
    alc1_tob1 <- rr[ , alc1_tob1]

    si <- (alc1_tob1 - 1) / ((alc1_tob0 - 1) + (alc0_tob1 - 1))

  } else {

    si <- 1

  }

  # Calculate when to apply the synergy index to an individual.
  dtmp[ , si.indiv := ifelse(x.tob == "current" & x.alc > 0, si, 1)]


# Final output
return(dtmp[ , si.indiv])
}



