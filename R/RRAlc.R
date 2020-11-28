

#' Relative risks for alcohol related diseases \lifecycle{stable}
#'
#' Computes the relative risks for each alcohol related disease based on the published risk curves.
#'
#' Relative risks for partially attributable chronic come from published risk functions whose parameters have been
#' hard-coded within this function rather than being read from an external spreadsheet. For some conditions there are
#' separate risk functions for morbidity and mortality. For conditions that show a J-shaped risk function that
#' indicates protective effects of alcohol, there is an option to remove the protective effect by setting all
#' RR < 1 = 1.   
#' 
#' Relative risks for partially attributable acute are computed by the PArisk function called from within
#'  this function. The characteristics of individual single occassion drinking are also calculated within this function 
#'  using AlcBinge_stapm().     
#'  
#'  Relative risks for wholly attributable chronic and wholly attributable acute conditions are calculated
#'  based on the extent to which either weekly or daily consumption exceeds a pre-specified threshold. The risk 
#'  for wholly attributable acute conditions is calculated by the function WArisk_acute(). We developed a new 
#'  method to model the absolute risk of wholly attributable acute conditions to suit the STAPM modelling. 
#'  This new method is based on the method used to model the risk of partially attributable acute conditions - 
#'  the shape of the risk function is determined by the individual variation in the total annual number of units that
#'   are drunk above the male/female thresholds for single ocassion binge drinking.  
#'
#' @param data Data table of individual characteristics.
#' @param disease Character - the name of the disease for which the relative risks will be computed.
#' @param av_weekly_grams_per_day_var Character - the name of the variable containing each individual's
#' average weekly consumption of alcohol in grams of ethanol per day.
#' @param sex_var Character - the name of the variable containing individual sex.
#' @param age_var Character - the name of the variable containing individual age in single years.
#' @param mort_or_morb Character - for alcohol related diseases that have separate
#' relative risk curves for mortality and morbidity, should the curve corresponding to
#'  mortality ("mort") or morbidity ("morb") be used.
#' @param getcurve Logical - do you just want to look at the risk function curve?
#' @template alc-epi-args
#'
#' @return Returns a numeric vector of each individual's relative risks for the alcohol related disease specified by "disease".
#' @importFrom data.table := setDT setnames
#' @export
#' 
#'
#' @examples
#'
#'\dontrun{
#'
#' # Draw disease specific risk functions
#'
#' # Example data
#' data <- data.table(
#'   GPerDay = 0:100,
#'   #peakday_grams = 0:100,
#'   sex = "Female",
#'   age = 30
#' )
#'
#' # Apply the function
#' test1 <- RRalc(
#'   data,
#'   disease = "Pharynx",
#'   mort_or_morb = "mort"
#' )
#'
#' test2 <- RRalc(
#'   data,
#'   disease = "Ischaemic_heart_disease",
#'   mort_or_morb = "morb"
#' )
#'
#' test3 <- RRalc(
#'   data,
#'   disease = "LiverCirrhosis",
#'   mort_or_morb = "mort"
#' )
#'
#' # Plot the risk functions
#' plot(test1 ~ I(0:100), type = "l", ylim = c(0, 10), ylab = "rr", 
#' main = "Females, age 30", xlab = "g per day")
#' lines(test2 ~ I(0:100), col = 2)
#' lines(test3 ~ I(0:100), col = 3)
#' legend("topleft", 
#' c("Pharyngeal cancer", "Ischaemic heart disease morbidity", "Liver Cirrhosis mortality"), 
#' lty = 1, col = 1:3)
#'}
RRalc <- function(
  data,
  disease = "Pharynx",
  av_weekly_grams_per_day_var = "GPerDay",
  sex_var = "sex",
  age_var = "age",
  mort_or_morb = c("mort", "morb"),
  alc_protective = TRUE,
  alc_wholly_chronic_thresholds = c(2, 2),
  alc_wholly_acute_thresholds = c(3, 4),
  grams_ethanol_per_unit = 8,
  getcurve = F
) {
  
  n <- nrow(data)
  
  x <- data[ , get(av_weekly_grams_per_day_var)]
  #p <- data[ , get(peak_grams_per_day_var)] # old code when used peakday directly from HSE
  sex <- data[ , get(sex_var)]
  age <-  data[ , get(age_var)]
  
  # Convert age in single years into categories
  age <- c("<16", "16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89", "90+")[
    findInterval(age, c(-1, 16, 18, 25, 35, 50, 65, 75, 90))]
  
  # Create the vector of relative risks to be returned
  # Initially set everyone's value to 1
  risk_indiv <- rep(1, n)
  
  if(getcurve == FALSE) {
    
    # Estimate the characteristics of single occassion drinking
    # based on the coefficients from Hill-McManus et al 2014
    data <- tobalcepi::AlcBinge_stapm(data)
    
  }
  
  ################################################################################
  # Partial chronic--------
  
  
  if(getcurve == FALSE) {
    
    # Calculate average amount drunk per drinking occassion
    p <- grams_ethanol_per_unit * data[ , mean_sod] / data[ , drink_freq]
    
  }
  
  ###########
  # Cancers #
  ###########
  
  
  # Cancer of the oral cavity and pharynx----
  # BAGNARDI, V., ROTA, M., BOTTERI, E. et al. (2015) Alcohol consumption and site-specific cancer risk: a comprehensive dose-response meta-analysis, British Journal of Cancer, 112, 580-593
  
  if(disease %in% c("Pharynx", "Oral_cavity", "Pharynx_and_Oral_cavity", "Oropharyngeal")) {
    
    b1 <- 0.02474
    b2 <- 0.00004
    
    risk_indiv <- exp(b1 * x - b2 * (x^2))
    
  }
  
  # Cancer of the oesophagus----
  # BAGNARDI, V., ROTA, M., BOTTERI, E. et al. (2015) Alcohol consumption and site-specific cancer risk: a comprehensive dose-response meta-analysis, British Journal of Cancer, 112, 580-593
  
  if(disease %in% c("Oesophagus", "Oesophageal", "Oesophageal_SCC")) {
    
    b1 <- 0.05593
    b2 <- 0.00789
    
    risk_indiv <- exp(b1 * x - b2 * x * log(x))
    
  }
  
  # Cancer of the colon and rectum----
  # BAGNARDI, V., ROTA, M., BOTTERI, E. et al. (2015) Alcohol consumption and site-specific cancer risk: a comprehensive dose-response meta-analysis, British Journal of Cancer, 112, 580-593
  
  if(disease == "Colorectal") {
    
    b1 <- 0.006279
    
    risk_indiv <- exp(b1 * x)
    
  }
  
  # Cancer of the liver and intrahepatic bile ducts----
  # Chuang et al 2015
  
  if(disease == "Liver") {
    
    b1 <- 0.4100701
    y <- (x + 12) / 100
    b2 <- 0.6728571429
    b3 <- 0.6101417
    b4 <- 0.4527367347
    b5 <- 0.4939596
    
    risk_indiv <- exp(b1 * (y - b2) + b3 * ((y^2) - b4) + b5)
    
  }
  
  # Cancer of the pancreas----
  # BAGNARDI, V., ROTA, M., BOTTERI, E. et al. (2015) Alcohol consumption and site-specific cancer risk: a comprehensive dose-response meta-analysis, British Journal of Cancer, 112, 580-593
  
  if(disease %in% c("Pancreas", "Pancreatic")) {
    
    b1 <- 0.002089
    
    risk_indiv <- exp(b1 * x)
    
  }
  
  # Cancer of the larynx----
  # BAGNARDI, V., ROTA, M., BOTTERI, E. et al. (2015) Alcohol consumption and site-specific cancer risk: a comprehensive dose-response meta-analysis, British Journal of Cancer, 112, 580-593
  
  if(disease %in% c("Larynx", "Laryngeal")) {
    
    b1 <- 0.01462
    b2 <- 0.00002
    
    risk_indiv <- exp(b1 * x - b2 * (x^2))
    
  }
  
  # Cancer of the breast----
  # BAGNARDI, V., ROTA, M., BOTTERI, E. et al. (2015) Alcohol consumption and site-specific cancer risk: a comprehensive dose-response meta-analysis, British Journal of Cancer, 112, 580-593
  
  if(disease == "Breast") {
    
    b1 <- 0.01018
    
    risk_indiv <- exp(b1 * x)
    
    risk_indiv[sex == "Male"] <- 1
    
  }
  
  
  ##################
  # Cardiovascular #
  ##################
  
  
  # Hypertensive heart disease----
  # Roerecke et al. (in press)
  
  if(disease == "HypertensiveHeartDisease") {
    
    # Male
    m1 <- 0.0150537
    m2 <- 0.0156155
    
    rr.ma <- exp(m1 * x - m2 * (x^3) / (75^2))
    rr.mb <- exp(m1 * x - m2 * (((x^3) - ((x - 21)^3 * 75) / 54) / (75^2)))
    rr.mc <- exp(m1 * x - m2 * ((x^3) - ((x - 21)^3 * 75 - (x - 75)^3 * 21) / 54) / (75^2))
    
    rr.m <- ifelse(x < 21, rr.ma, ifelse(x >= 21 & x < 75, rr.mb, rr.mc))
    
    # Female
    f1 <- 0
    f2 <- 0.0154196
    f3 <- 0.0217586
    f4 <- 0.9649937
    
    rr.fa <- exp(f1)
    rr.fb <- exp(-f2 * x + f3 * (x^3 - ((x - 10)^3 * 20 - (x - 20)^3 * 10) / 10) / 20^2)
    rr.fc <- exp(f4)
    
    rr.f <- ifelse(x < 18.9517, rr.fa, ifelse(x >= 18.9517 & x < 75, rr.fb, rr.fc))
    
    # Combine
    risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
    
  }
  
  
  
  # Ischaemic heart disease----
  
  if(tolower(disease) %in% c("ischaemic_heart_disease", "ischaemic_heart_disease_morb")) {
    
    # Mortality
    # REHM, J., SHIELD, K. D., ROERECKE, M. & GMEL, G. (2016) Modelling the impact of alcohol consumption on cardiovascular disease mortality for comparative risk assessments: an overview BMC Public Health, 16, 363
    
    if(mort_or_morb == "mort") {
      
      y <- (x + 0.0099999997764826) / 100
      
      b1 <- 1.111874 # 16-34
      b2 <- 1.035623 # 35-64
      b3 <- 0.757104 # 65+
      
      
      # Male
      
      m1 <- 0.4870068
      m2 <- 1.550984
      m3 <- 0
      m4 <- 0.012
      
      # 16-34
      rr.ma1 <- exp(b1 * (-m1 * sqrt(y) + m2 * y^3))
      rr.mb1 <- exp(m3)
      rr.mc1 <- exp(m4 * (x - 100))
      
      rr.m1 <- ifelse(x <= 60, rr.ma1, ifelse(x > 60 & x < 100, rr.mb1, rr.mc1))
      
      # 35-64
      rr.ma2 <- exp(b2 * (-m1 * sqrt(y) + m2 * y^3))
      rr.mb2 <- exp(m3)
      rr.mc2 <- exp(m4 * (x - 100))
      
      rr.m2 <- ifelse(x <= 60, rr.ma2, ifelse(x > 60 & x < 100, rr.mb2, rr.mc2))
      
      # 65+
      rr.ma3 <- exp(b3 * (-m1 * sqrt(y) + m2 * y^3))
      rr.mb3 <- exp(m3)
      rr.mc3 <- exp(m4 * (x - 100))
      
      rr.m3 <- ifelse(x <= 60, rr.ma3, ifelse(x > 60 & x < 100, rr.mb3, rr.mc3))
      
      
      rr.m <- ifelse(age %in% c("<16", "16-17", "18-24", "25-34"), rr.m1,
                     ifelse(age %in% c("35-49", "50-64"), rr.m2,
                            ifelse(age %in% c("65-74", "75-89"), rr.m3, NA)))
      
      # Female
      
      f1 <- 1.832441
      f2 <- 1.538557
      f3 <- 0.01
      f4 <- 0.0093
      f5 <- 0.0068
      f6 <- 30.3814
      
      # 16-34
      rr.fa1 <- exp(b1 * (f1 * y + f2 * y * log(y)))
      rr.fb1 <- exp(f3 * (x - f6))
      
      rr.f1 <- ifelse(x < f6, rr.fa1, rr.fb1)
      
      # 35-64
      rr.fa2 <- exp(b2 * (f1 * y + f2 * y * log(y)))
      rr.fb2 <- exp(f4 * (x - f6))
      
      rr.f2 <- ifelse(x < f6, rr.fa2, rr.fb2)
      
      # 65+
      rr.fa3 <- exp(b3 * (f1 * y + f2 * y * log(y)))
      rr.fb3 <- exp(f5 * (x - f6))
      
      rr.f3 <- ifelse(x < f6, rr.fa3, rr.fb3)
      
      
      rr.f <- ifelse(age %in% c("<16", "16-17", "18-24", "25-34"), rr.f1,
                     ifelse(age %in% c("35-49", "50-64"), rr.f2,
                            ifelse(age %in% c("65-74", "75-89"), rr.f3, NA)))
      
      
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
      if(!isTRUE(getcurve)) {
        
        # remove protective effect for people who binge drink > 60 g/day
        risk_indiv[risk_indiv < 1 & p > 60] <- 1
        
      }
      
    }
    
    
    # Morbidity
    # ROERECKE, M., & REHM, J. (2012). The cardioprotective association of average alcohol consumption and ischaemic heart disease: a systematic review and meta‐analysis. Addiction, 107(7), 1246-1260.
    # All protective effects removed for binge drinkers (>60g/day)
    # ROERECKE, M., & REHM, J. (2010). Irregular heavy drinking occasions and risk of ischemic heart disease: a systematic review and meta-analysis. American journal of epidemiology, 171(6), 633-644
    
    
    if(mort_or_morb == "morb") {
      
      # Male
      
      m1 <- 0.1178113
      m2 <- 0.0189
      
      rr.ma <- exp(-m1 * sqrt(x) + m2 * sqrt(x) * log(x))
      rr.mb <- exp(0)
      
      rr.m <- ifelse(x < 60, rr.ma, rr.mb)
      
      
      # Female
      
      f1 <- 0.296842
      f2 <- 0.0392805
      
      rr.f <- exp(-f1 * sqrt(x) + f2 * x)
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
      if(!isTRUE(getcurve)) {
        
        # remove protective effect for people who binge drink > 60 g/day
        risk_indiv[risk_indiv < 1 & p > 60] <- 1
        
      }
      
    }
    
    if(!isTRUE(alc_protective)) {
      
      risk_indiv[risk_indiv < 1] <- 1
      
    }
    
  }
  
  
  # Cardiac arrhythmias----
  # SAMOKHVALOV A. V., IRVING H. M., REHM J. Alcohol as a risk factor for atrial fibrillation: a systematic review and meta-analysis. Eur J Cardiovasc Prev Rehabil 2010; 17: 706–712
  
  if(disease == "Cardiac_Arrhythmias") {
    
    b1 <- 0.0575183
    y <- (x + 0.0499992370605469) / 10
    
    risk_indiv <- exp(b1 * y)
    
  }
  
  
  
  # Haemorrhagic and other non-ischaemic stroke----
  # PATRA, J., TAYLOR, B., IRVING, H. et al. (2010) Alcohol consumption and the risk of morbidity and mortality for different stroke types--a systematic review and meta-analysis, BMC Public Health, 10, 258
  
  if(tolower(disease) %in% c("haemorrhagic_stroke", "haemorrhagic_stroke_morb")) {
    
    # Mortality
    
    if(mort_or_morb == "mort") {
      
      # Male
      
      m1 <- 1.006943
      m2 <- 0.6898937
      m3 <- 0.0028572082519531
      
      rr.ma <- exp(log(1 - x * (1 - m1)))
      rr.mb <- exp(m2 * ((x + m3) / 100))
      
      rr.m <- ifelse(x <= 1, rr.ma, rr.mb)
      
      # Female
      
      f1 <- 1.014815
      f2 <- 1.466406
      f3 <- 0.0028572082519531
      
      rr.fa <- exp(log(1 - x * (1 - f1)))
      rr.fb <- exp(f2 * ((x + f3) / 100))
      
      rr.f <- ifelse(x <= 1, rr.fa, rr.fb)
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
    }
    
    
    # Morbidity
    
    if(mort_or_morb == "morb") {
      
      # Male
      
      m1 <- 0.007695021
      
      rr.m <- exp(x * m1)
      
      # Female
      
      f1 <- 0.340861
      f2 <- 0.0944208
      
      rr.f <- exp(-f1 * sqrt(x) + f2 * sqrt(x) * log(x))
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
    }
    
    if(!isTRUE(alc_protective)) {
      
      risk_indiv[risk_indiv < 1] <- 1
      
    }
    
  }
  
  
  # Ischaemic stroke----
  
  if(tolower(disease) %in% c("ischaemic_stroke", "ischaemic_stroke_morb")) {
    
    # Mortality
    # REHM, J., SHIELD, K. D., ROERECKE, M. & GMEL, G. (2016) Modelling the impact of alcohol consumption on cardiovascular disease mortality for comparative risk assessments: an overview BMC Public Health, 16, 363
    
    if(mort_or_morb == "mort") {
      
      a1 <- 1.111874
      a2 <- 1.035623
      a3 <- 0.757104
      
      m1 <- 0.4030081
      m2 <- 0.3877538
      
      f1 <- 2.48768
      f2 <- 3.708724
      
      e1 <- 0.03521
      e2 <- 0.03279
      e3 <- 0.02397
      e4 <- 0.37987
      e5 <- 0.35382
      e6 <- 0.25866
      
      y <- (x + 0.0028572082519531) / 100
      
      
      # Male
      
      # 16-34
      rr.ma1 <- 1 - x * (1 - exp(-e1))
      rr.mb1 <- exp(a1 * (m1 * sqrt(y) + m2 * sqrt(y) * log(y)))
      
      rr.m1 <- ifelse(x <= 1, rr.ma1, rr.mb1)
      
      # 35-64
      rr.ma2 <- 1 - x * (1 - exp(-e2))
      rr.mb2 <- exp(a2 * (m1 * sqrt(y) + m2 * sqrt(y) * log(y)))
      
      rr.m2 <- ifelse(x <= 1, rr.ma2, rr.mb2)
      
      # 65+
      rr.ma3 <- 1 - x * (1 - exp(-e3))
      rr.mb3 <- exp(a3 * (m1 * sqrt(y) + m2 * sqrt(y) * log(y)))
      
      rr.m3 <- ifelse(x <= 1, rr.ma3, rr.mb3)
      
      rr.m <- ifelse(age %in% c("<16", "16-17", "18-24", "25-34"), rr.m1,
                     ifelse(age %in% c("35-49", "50-64"), rr.m2,
                            ifelse(age %in% c("65-74", "75-89"), rr.m3, NA)))
      
      # Female
      
      # 16-34
      rr.fa1 <- 1 - x * (1 - exp(-e4))
      rr.fb1 <- exp(a1 * (-f1 * sqrt(y) + f2 * y))
      
      rr.f1 <- ifelse(x <= 1, rr.fa1, rr.fb1)
      
      # 35-64
      rr.fa2 <- 1 - x * (1 - exp(-e5))
      rr.fb2 <- exp(a2 * (-f1 * sqrt(y) + f2 * y))
      
      rr.f2 <- ifelse(x <= 1, rr.fa2, rr.fb2)
      
      # Female, 65+
      rr.fa3 <- 1 - x * (1 - exp(-e6))
      rr.fb3 <- exp(a3 * (-f1 * sqrt(y) + f2 * y))
      
      rr.f3 <- ifelse(x <= 1, rr.fa3, rr.fb3)
      
      rr.f <- ifelse(age %in% c("<16", "16-17", "18-24", "25-34"), rr.f1,
                     ifelse(age %in% c("35-49", "50-64"), rr.f2,
                            ifelse(age %in% c("65-74", "75-89"), rr.f3, NA)))
      
      if(!isTRUE(getcurve)) {
        
        # remove protective effect for people who binge drink > 60 g/day
        risk_indiv[risk_indiv < 1 & p > 60] <- 1
        
      }
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
    }
    
    
    # Morbidity
    # PATRA, J., TAYLOR, B., IRVING, H. et al. (2010) Alcohol consumption and the risk of morbidity and mortality for different stroke types--a systematic review and meta-analysis, BMC Public Health, 10, 258
    # All protective effects removed for binge drinkers (>60g/day)
    # REHM, J., SHIELD, K. D., ROERECKE, M. & GMEL, G. (2016) Modelling the impact of alcohol consumption on cardiovascular disease mortality for comparative risk assessments: an overview BMC Public Health, 16, 363
    
    if(mort_or_morb == "morb") {
      
      # Male
      
      m1 <- 0.132894
      m2 <- 0.03677422
      
      rr.m <- exp(-m1 * sqrt(x) + m2 * sqrt(x) * log(x))
      
      # Female
      
      f1 <- 0.114287
      f2 <- 0.01680936
      
      rr.f <- exp(-f1 * sqrt(x) + f2 * x)
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
      if(!isTRUE(getcurve)) {
        
        # remove protective effect for people who binge drink > 60 g/day
        risk_indiv[risk_indiv < 1 & p > 60] <- 1
        
      }
      
    }
    
    if(!isTRUE(alc_protective)) {
      
      risk_indiv[risk_indiv < 1] <- 1
      
    }
    
  }
  
  
  
  
  #############
  # Digestive #
  #############
  
  
  # Fibrosis and cirrhosis of the liver----
  # REHM, J., TAYLOR, B., MOHAPATRA, S. et al. (2010) Alcohol as a risk factor for liver cirrhosis: a systematic review and meta-analysis, Drug and Alcohol Review, 29, 437-45
  
  if(tolower(disease) %in% c("livercirrhosis", "livercirrhosis_morb")) {
    
    
    # Mortality
    
    if(mort_or_morb == "mort") {
      
      y <- (x + 0.1699981689453125) / 100
      
      # Male
      
      m1 <- 1.033224
      m2 <- 2.793524
      
      rr.ma <- exp(log(1 + x * (m1 - 1)))
      rr.mb <- exp(m2 * y)
      
      rr.m <- ifelse(x <= 1, rr.ma, rr.mb)
      
      # Female
      
      f1 <- 1.421569
      f2 <- 3.252035
      
      rr.fa <- exp(log(1 + x * (f1 - 1)))
      rr.fb <- exp(f2 * sqrt(y))
      
      rr.f <- ifelse(x <= 1, rr.fa, rr.fb)
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
    }
    
    # Morbidity
    
    if(mort_or_morb == "morb") {
      
      # Male
      
      m1 <- 0.01687111
      
      rr.m <- exp(m1 * x)
      
      # Female
      
      f1 <- 0.2351821
      
      rr.f <- exp(f1 * sqrt(x))
      
      # Combine
      risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
      
    }
    
  }
  
  
  # Acute pancreatitis----
  # SAMOKHVALOV, A. V., REHM, J. & ROERECKE, M. (2015) Alcohol consumption as a risk factor for acute and chronic pancreatitis: a systematic review and a series of meta-analyses, EBioMedicine, 2, 1996-2002
  
  if(disease == "Acute_Pancreatitis") {
    
    # Male
    
    m1 <- 0.013
    
    rr.m <- exp(m1 * x)
    
    # Female
    
    f1 <- 0.0272886
    f2 <- 0.0611466
    f3 <- 2.327965
    
    rr.f1 <- exp(-f1 * x)
    rr.f2 <- exp(-f1 * x + f2 * ((x - 3)^3) / ((40 - 3)^2))
    rr.f3 <- exp(-f1 * x + f2 * (  ((x - 3)^3) - (  (  ((x - 15)^3) * (40 - 3)  ) / (40 - 15)  )  ) / ((40 - 3)^2)  )
    rr.f4 <- exp(-f1 * x + f2 * (  ((x - 3)^3) - (  (  ((x - 15)^3) * (40 - 3) - ((x - 40)^3) * (15 - 3)  ) / (40 - 15)  )  ) / ((40 - 3)^2)  )
    rr.f5 <- exp(f3)
    
    rr.f <- ifelse(x < 3, rr.f1,
                   ifelse(x >= 3 & x < 15, rr.f2,
                          ifelse(x >= 15 & x < 40, rr.f3,
                                 ifelse(x >= 40 & x < 108, rr.f4,
                                        ifelse(x >= 108, rr.f5, NA)))))
    
    # Combine
    risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
    
    if(!isTRUE(alc_protective)) {
      
      risk_indiv[risk_indiv < 1] <- 1
      
    }
    
  }
  
  # Chronic pancreatitis----
  # SAMOKHVALOV, A. V., REHM, J. & ROERECKE, M. (2015) Alcohol consumption as a risk factor for acute and chronic pancreatitis: a systematic review and a series of meta-analyses, EBioMedicine, 2, 1996-2002
  
  if(disease == "Chronic_Pancreatitis") {
    
    risk_indiv <- exp(0.018 * x)
    
  }
  
  
  # Acute and Chronic pancreatitis----
  # Irving et al 2009
  # this is the old version of the pancreatitis risk function
  # in the newer version of the model it has been replaced by separate risk functions for acute and chronic
  # included here as needed for alc costs to pc work that was based on the old disease list
  # that grouped acute and chronic together
  
  if(disease == "Acute_and_Chronic_Pancreatitis") {
    
    risk_indiv <- exp(1.259e-5 + x * 8.67933e-5 + (x^2) * 0.00015)
    
  }
  
  
  
  
  #############
  # Endocrine #
  #############
  
  
  # Type II Diabetes----
  # KNOTT, C., BELL, S., & BRITTON, A. (2015). Alcohol consumption and the risk of type 2 diabetes: a systematic review and dose-response meta-analysis of more than 1.9 million individuals from 38 observational studies. Diabetes care, 38(9), 1804-1812.
  
  if(disease == "Diabetes") {
    
    # Male
    m1 <- 0.00001763703
    m2 <- 0.0000000728256
    
    rr.m <- exp(m1 * (x^2) - m2 * (x^3))
    
    # Female
    f1 <- 0.1313991
    f2 <- 0.01014239
    
    rr.f <- exp(-f1 * sqrt(x) + f2 * x)
    
    # Combine
    risk_indiv <- ifelse(sex == "Male", rr.m, ifelse(sex == "Female", rr.f, NA))
    
    if(!isTRUE(alc_protective)) {
      
      risk_indiv[risk_indiv < 1] <- 1
      
    }
    
  }
  
  
  
  
  ##################
  # Nervous system #
  ##################
  
  
  # Epilepsy----
  # SAMOKHVALOV, A. V., IRVING, H., MOHAPATRA, S. & REHM, J. (2010) Alcohol consumption, unprovoked seizures and epilepsy: a systematic review and meta-analysis, Epilepsia, 51, 1177-1184
  
  if(disease == "Epilepsy") {
    
    risk_indiv <- exp(1.22861 * (x + 0.5) / 100)
    
  }
  
  
  
  
  ###############
  # Respiratory #
  ###############
  
  # Tuberculosis----
  # IMTIAZ, S., SHIELD, K. D., ROERECKE, M., SAMOKHVALOV, A.V., LONNROTH, K., REHM, J. (2017) Alcohol consumption as a risk factor fortuberculosis: meta-analyses and burden of disease. European Respiratory Journal, 50(1), 1700216
  
  if(disease == "Tuberculosis") {
    
    risk_indiv <- exp(0.0179695 * x)
    
  }
  
  # Lower respiratory tract infections / Pneumonia----
  # SAMOKHVALOV, A. V., IRVING, H. M. & REHM, J. (2010) Alcohol consumption as a risk factor for pneumonia: systematic review and meta-analysis, Epidemiology and Infection, 138, 1789-1795
  
  if(disease %in% c("Pneumonia", "Influenza_clinically_diagnosed", "Influenza_microbiologically_confirmed", "Lower_respiratory_tract_infections")) {
    
    risk_indiv <- exp(0.4764038 * (x + 0.0399999618530273) / 100)
    
  }
  
  # Just to be sure - and fix errors due to log(0) = -Inf
  risk_indiv[x == 0] <- 1
  
  ################################################################################
  # Partial acute-------
  
  
  # Transport injuries----
  
  if(disease == "Transport_injuries") {
    
    data[ , rr := sapply(1:n, function(z) {
      
      tobalcepi::PArisk(
        SODMean = mean_sod[z],
        SODSDV = occ_sd[z],
        SODFreq = drink_freq[z],
        Weight = weight[z],
        Widmark_r = rwatson[z],
        cause = "Transport",
        grams_ethanol_per_unit = grams_ethanol_per_unit
      )
    })]
    
    risk_indiv <- data[ , rr]
    
    data[ , rr := NULL]
    
  }
  
  
  # Fall injuries----
  
  if(disease == "Fall_injuries") {
    
    data[ , rr := sapply(1:n, function(z) {
      tobalcepi::PArisk(
        SODMean = mean_sod[z],
        SODSDV = occ_sd[z],
        SODFreq = drink_freq[z],
        Weight = weight[z],
        Widmark_r = rwatson[z],
        cause = "Fall",
        grams_ethanol_per_unit = grams_ethanol_per_unit
      )
    })]
    
    risk_indiv <- data[ , rr]
    
    data[ , rr := NULL]
    
  }
  
  # Violence----
  
  if(disease %in% c("Assault", "Other_intentional_injuries")) {
    
    data[ , rr := sapply(1:n, function(z) {
      tobalcepi::PArisk(
        SODMean = mean_sod[z],
        SODSDV = occ_sd[z],
        SODFreq = drink_freq[z],
        Weight = weight[z],
        Widmark_r = rwatson[z],
        cause = "Violence",
        grams_ethanol_per_unit = grams_ethanol_per_unit
      )
    })]
    
    risk_indiv <- data[ , rr]
    
    data[ , rr := NULL]
    
  }
  
  # Other----
  
  if(disease %in% c("Mechanical_forces", "Drowning", "Other_unintentional_injuries", "intentional_self_harm", "Accidental_poisoning", "Fire_injuries")) {
    
    data[ , rr := sapply(1:n, function(z) {
      tobalcepi::PArisk(
        SODMean = mean_sod[z],
        SODSDV = occ_sd[z],
        SODFreq = drink_freq[z],
        Weight = weight[z],
        Widmark_r = rwatson[z],
        cause = "Other",
        grams_ethanol_per_unit = grams_ethanol_per_unit
      )
    })]
    
    risk_indiv <- data[ , rr]
    
    data[ , rr := NULL]
    
  }
  
  #data[ , `:=`(mean_sod = NULL, occ_sd = NULL, drink_freq = NULL, weight = NULL, rwatson = NULL)]
  
  ################################################################################
  # Wholly attributable acute--------
  
  # Calculate the absolute rather than the relative risk
  
  if(disease %in% c(
    "Excessive_Blood_Level_of_Alcohol",
    "Toxic_effect_of_alcohol",
    "Alcohol_poisoning",
    "Evidence_of_alcohol_involvement_determined_by_blood_alcohol_level",
    "Acute_intoxication")
  ) {
    
    # Old method - replicating SAPM
    
    #data[sex == "Female", threshold := alc_wholly_acute_thresholds[1]]
    #data[sex == "Male", threshold := alc_wholly_acute_thresholds[2]]
    
    #data[ , ar := 0]
    #data[ , diff := p - threshold]
    ##data[diff > 0, ar := diff / grams_ethanol_per_unit]
    #data[diff > 0, ar := diff]
    
    #risk_indiv <- 1 + data[ , ar] # add 1 to remove 0/0 = Not a number error later
    
    #data[ , `:=`(ar = NULL, threshold = NULL, diff = NULL)]
    
    ###############################
    
    # New method for STAPM
    # based on the method used for partially attributable chronic conditions
    
    #test <- copy(data)
    
    #SODMean = test[1, mean_sod]
    #SODSDV = test[1, occ_sd]
    #SODFreq = test[1, drink_freq]
    #sex = test[1 , sex]
    #grams_ethanol_per_unit = grams_ethanol_per_unit
    #alc_wholly_acute_thresholds = alc_wholly_acute_thresholds
    
    data[ , ar := sapply(1:n, function(z) {
      tobalcepi::WArisk_acute(
        SODMean = mean_sod[z],
        SODSDV = occ_sd[z],
        SODFreq = drink_freq[z],
        sex = sex[z],
        grams_ethanol_per_unit = grams_ethanol_per_unit,
        alc_wholly_acute_thresholds = alc_wholly_acute_thresholds
      )
    })]
    
    risk_indiv <- 1 + data[ , ar] # add 1 to remove 0/0 = Not a number error later
    
    data[ , ar := NULL]
    
  }
  
  
  ################################################################################
  # Wholly attributable chronic--------
  
  # Calculate the absolute rather than the relative risk
  
  if(disease %in% c(
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
    
    # Assign the drinking thresholds over which harm occurs, in grams of ethanol per day
    data[sex == "Female", threshold := alc_wholly_chronic_thresholds[1] * grams_ethanol_per_unit]
    data[sex == "Male", threshold := alc_wholly_chronic_thresholds[2] * grams_ethanol_per_unit]
    
    data[ , ar := 0]
    data[ , diff := x - threshold]
    #data[diff > 0, ar := diff  * (7 / grams_ethanol_per_unit)]
    data[diff > 0, ar := diff]
    
    risk_indiv <- 1 + data[ , ar]
    
    data[ , `:=`(ar = NULL, threshold = NULL, diff = NULL)]
    
  }
  
  
  data[ , `:=`(mean_sod = NULL, occ_sd = NULL, drink_freq = NULL, weight = NULL, rwatson = NULL)]
  
  
  return(risk_indiv)
}







