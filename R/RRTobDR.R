#' Dose-response relative risks for tobacco-related cancers
#' 
#' \lifecycle{experimental}
#'
#' Computes the relative risks for each tobacco-related cancer based on the published risk curves.
#'
#' Relative risks for come from published risk functions whose parameters have been
#' hard-coded within this function rather than being read from an external spreadsheet. 
#' These relative risks are based on an individual's current smoking intensity. There are 
#' others measures of smoking exposure including smoking duration and pack-years, which 
#' we will come to think about further.
#'
#' @param data Data table of individual characteristics.
#' @param disease Character - the name of the disease for which the relative risks will be computed.
#' @param av_cigs_day Character - the name of the variable containing each individual's
#' average number of daily cigarettes.
#' 
#' @return Returns a numeric vector of each individual's relative risks for the tobacco related disease specified by "disease".
#' @importFrom data.table := setDT setnames
#' @export
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' RRTobDR(data = data,
#'        disease = "Pharynx",
#'        av_cigs_day = "cigs_per_day"
#'        )
#'
#' }
RRTobDR <- function(
  data,
  disease = "Pharynx",
  av_cigs_day = "cigs_per_day"
) {
  
  n <- nrow(data)
  
  x <- data[ , get(av_cigs_day)]
  
  # Create the vector of relative risks to be returned
  # Initially set everyone's value to 1
  risk_indiv <- rep(1, n)
  
  
  ###########
  # Cancers #
  ###########
  
  # Pancreatic cancer----
  # Lugo A, Peveri G, Bosetti C, Bagnardi V, Crippa A, Orsini N, et al. Strong excess risk of pancreatic cancer for low frequency and duration of cigarette smoking: A comprehensive review and meta-analysis. European Journal of Cancer. 104:117-26. PubMed PMID: 30347287.
  
  if(disease %in% c("Pancreatic", "Pancreas")) {
  
    rr.a <- exp(-0.0000612*(x^3) + 0.0558513*(x))
    rr.b <- exp(0.0000314*(x^3) - 0.0027755*(x^2) + 0.0836061*(x) - 0.0925158)
    rr.c <- exp(0.00172953*(x) + 0.712603)
    
    risk_indiv <- ifelse(x < 10, rr.a, ifelse(x >= 10 & x < 29.5, rr.b, rr.c))
    
  }
  
  
  # Kidney cancer 
  #Liu X, Peveri G, Bosetti C, Bagnardi V, Specchia C, Gallus S, Lugo A. Dose-response relationships between cigarette smoking and kidney cancer: a systematic review and meta-analysis. Critical reviews in oncology/hematology. 2019 Jul 29.
  
  if(disease %in% c("Kidney")){
    
    rr.a <- exp(-0.00002795*(x^3) + 0.0337577*(x))
    rr.b <- exp(0.00001692*(x^3) - 0.00154797*(x^2) + 0.0515594*(x) - 0.0682697)
    rr.c <- exp(0.00434628*(x) + 0.41176)
    
    risk_indiv <- ifelse(x < 11.5, rr.a, ifelse(x >= 11.5 & x < 30.5, rr.b, rr.c))
    
    
  }
  
  
  # Laryngeal cancer 
  #ZUO JJ, TAO ZZ, CHEN C, HU ZW, XU YX, ZHENG AY, GUO Y. Characteristics of cigarette smoking without alcohol consumption and laryngeal cancer: overall and time-risk relation. A meta-analysis of observational studies. European Archives of Oto-Rhino-Laryngology. 2017 Mar 1;274(3):1617-31.
  if(disease %in% c("Laryngeal", "Larynx")){
    rr.a <- 5.43
    rr.b <- 5.37
    rr.c <- 7.02
    
    risk_indiv <- ifelse(x < 20, rr.a, ifelse(x >= 20 & x < 30, rr.b, rr.c))
    
  }
  
  
  # Liver cancer 
  # LEE YC, COHET C, YANG YC, STAYNER L, HASHIBE M, STRAIF K. Meta-analysis of epidemiologic studies on cigarette smoking and liver cancer. International journal of epidemiology. 2009 Dec 1;38(6):1497-511.
  if(disease %in% c("Liver")){

    risk_indiv <- 1.071^(x/10)
    
  }
  
  
  
  # Acute myeloid leukaemia 
  # FIRCANIS S, MERRIAM P, KHAN N, CASTILLO JJ. The relation between cigarette smoking and risk of acute myeloid leukemia: An updated meta‐analysis of epidemiological studies. American journal of hematology. 2014 Aug;89(8):E125-32.
  if(disease %in% c("Acute_myeloid_leukaemia")) {
    
  rr.a <- 1.27
  rr.b <- 1.36
  rr.c <- 1.55
  rr.d <- 1.77
  
  risk_indiv <- ifelse(x < 10, rr.a, ifelse(x >= 10 & x < 20, rr.b, ifelse(x >= 20 & x < 30, rr.c, rr.d)))
  
  }
  
  # Colorectal 
  # LIANG PS, CHEN TY, GIOVANNUCCI E. Cigarette smoking and colorectal cancer incidence and mortality: systematic review and meta‐analysis. International journal of cancer. 2009 May 15;124(10):2406-15.
  if(disease %in% c("Colorectal")) {
  
  rr.a <- 1.137
  rr.b <- 1.293
  
  rr.c <- 1.410
  rr.d <- 1.989
  
  inc_indiv <- ifelse(x < 20, rr.a, rr.b)
  mort_indiv <- ifelse(x < 20, rr.c, rr.d)
  
  }
  
  
  # Lung cancer
  # GANDINI S, BOTTERI E, IODICE S, BONIOL M, LOWENFELS AB, MAISONNEUVE P, BOYLE P. Tobacco smoking and cancer: A meta‐analysis. International journal of cancer. 2008 Jan 1;122(1):155-64.
  if(disease %in% c("Lung")) {
    
  risk_indiv <- 1.07^x
  
  }
  
  
  # Oesophageal SCC 
  # HASHIBE M, BOFFETTA P, JANOUT V, ZARIDZE D, SHANGINA O, MATES D, SZESZENIA‐DABROWSKA N, BENCKO V, BRENNAN P. Esophageal cancer in Central and Eastern Europe: tobacco and alcohol. International journal of cancer. 2007 Apr 1;120(7):1518-22.
  if(disease %in% c("Oesophageal_SCC")) {
    
  
  scc.a <- 4.24
  scc.b <- 5.21
  scc.c <- 5.86
  scc.d <- 4.77
  
  risk_indiv <- ifelse(x < 10, scc.a, ifelse(x >= 10 & x < 20, scc.b, ifelse(x >= 20 & x < 30, scc.c, scc.d)))
  
  } 
  
  
  
  # Oesophageal AC 
  # HASHIBE M, BOFFETTA P, JANOUT V, ZARIDZE D, SHANGINA O, MATES D, SZESZENIA‐DABROWSKA N, BENCKO V, BRENNAN P. Esophageal cancer in Central and Eastern Europe: tobacco and alcohol. International journal of cancer. 2007 Apr 1;120(7):1518-22.
  if(disease %in% c("Oesophageal_AC")) {
  ac.a <- 1.93
  ac.b <- 2.04
  ac.c <- 3.24
  ac.d <- 3.04
  

  risk_indiv <- ifelse(x < 10, ac.a, ifelse(x >= 10 & x < 20, ac.b, ifelse(x >= 20 & x < 30, ac.c, ac.d)))
  
  }
  
  
  # Nasal cavity 
  # XUE WQ, QIN HD, RUAN HL, SHUGART YY, JIA WH. Quantitative association of tobacco smoking with the risk of nasopharyngeal carcinoma: a comprehensive meta-analysis of studies conducted between 1979 and 2011. American journal of epidemiology. 2013 Aug 1;178(3):325-38.
  if(disease %in% c("Nasal_cavity", "Nasopharynx_sinonasal")) {
    
  rr.a <- 1.31
  rr.b <- 1.98
  
  risk_indiv <- ifelse(x < 23, rr.a, rr.b)
  
  }
  
  
  # Lower urinary tract 
  # ZEEGERS MP, TAN FE, DORANT E, VAN DEN BRANDT PA. The impact of characteristics of cigarette smoking on urinary tract cancer risk: a meta‐analysis of epidemiologic studies. Cancer. 2000 Aug 1;89(3):630-9.
  if(disease %in% c("Lower_urinary_tract")) {
    
  
  rr.a <- 2.04 
  rr.b <- 3.15
  
  risk_indiv <- ifelse(x < 20, rr.a, rr.b)
  
  }
  
    
  # Cancer of the oral cavity----
  # MAASLAND DH, VAN DEN BRANDT PA, KREMER B, GOLDBOHM RA, SCHOUTEN LJ. Alcohol consumption, cigarette smoking and the risk of subtypes of head-neck cancer: results from the Netherlands Cohort Study. BMC cancer. 2014 Dec 1;14(1):187.
  if(disease %in% c("Oral_cavity", "Oropharyngeal")) {
    
    risk_indiv <- 1.20^(x/10)
    
  }
  
  # Stomach 
  # TREDANIEL J, BOFFETTA P, BUIATTI E, SARACCI R, HIRSCH A. Tobacco smoking and gastric cancer: review and meta‐analysis. International journal of cancer. 1997 Aug 7;72(4):565-73.
  if(disease %in% c("Stomach")) {
    
  rr.a <- 1.49 
  rr.b <- 1.67
  
  risk_indiv <- ifelse(x < 20, rr.a, rr.b)
  
  }
  

  
  return(risk_indiv)
}

