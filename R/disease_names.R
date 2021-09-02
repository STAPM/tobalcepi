
#' Names of tobacco-related diseases
#'
#' @importFrom Rdpack reprompt
#'
#' @docType data
#'
#' @format A character vector containing the names of the 52 disease categories 
#' of smoking related diseases \insertCite{webster2018risk;textual}{tobalcepi}.
#'
#' @references
#' \insertRef{webster2018risk}{tobalcepi}
#'
"tob_disease_names"

#' Names of alcohol-related diseases
#'
#' @importFrom Rdpack reprompt
#'
#' @docType data
#'
#' @format A character vector containing the names of the 48 disease categories 
#' of alcohol related diseases \insertCite{Angus2018;textual}{tobalcepi}.
#'
#' @references
#' \insertRef{Angus2018}{tobalcepi}
#'
"alc_disease_names"

#' Groupings of diseases into disease types
#'
#' @importFrom Rdpack reprompt
#'
#' @docType data
#'
#' @format A data.table containing disease groupings following the schemes used in 
#' the Royal College of Physicians report 'Hiding in Plain Sight' Chapter 3 \insertCite{RCP2018}{tobalcepi} 
#' and \insertCite{Angus2018;textual}{tobalcepi}. 
#' Also includes a column of formatted disease names and an indication of which diseases are related to tobacco only, alcohol only or both
#'
#' @references
#' \insertRef{RCP2018}{tobalcepi}    
#' \insertRef{Angus2018}{tobalcepi}
#'
"disease_groups"

