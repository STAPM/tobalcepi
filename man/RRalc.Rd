% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RRAlc.R
\name{RRalc}
\alias{RRalc}
\title{Relative risks for alcohol related diseases}
\usage{
RRalc(
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
  getcurve = FALSE,
  within_model = TRUE
)
}
\arguments{
\item{data}{Data table of individual characteristics.}

\item{disease}{Character - the name of the disease for which the relative risks will be computed.}

\item{av_weekly_grams_per_day_var}{Character - the name of the variable containing each individual's
average weekly consumption of alcohol in grams of ethanol per day.}

\item{sex_var}{Character - the name of the variable containing individual sex.}

\item{age_var}{Character - the name of the variable containing individual age in single years.}

\item{mort_or_morb}{Character - for alcohol related diseases that have separate
relative risk curves for mortality and morbidity, should the curve corresponding to
 mortality ("mort") or morbidity ("morb") be used.}

\item{alc_protective}{Logical - whether to include the protective effects of
alcohol in the risk function. Defaults to TRUE. If TRUE, then the part of the risk function < 1 is set to equal 1.}

\item{alc_wholly_chronic_thresholds}{Numeric vector - the thresholds in UK standard units of alcohol per day 
over which individuals begin to experience an elevated risk
 for chronic diseases that are wholly attributable to alcohol. Input in the order c(female, male). 
 Defaults to the current UK healthy drinking threshold of 14 units/week for females and males, or 2 units/day.}

\item{alc_wholly_acute_thresholds}{Numeric vector - the thresholds in UK standard units of alcohol /day over
which individuals begin to experience an elevated risk
for acute diseases that are wholly attributable to alcohol. Input in the form c(female, male). 
Defaults to 3 units/day for females and 4 units/day for males.}

\item{grams_ethanol_per_unit}{Numeric value giving the conversion factor for the number of grams of pure
ethanol in one UK standard unit of alcohol.}

\item{getcurve}{Logical - do you just want to look at the risk function curve?}

\item{within_model}{Logical - is the function being used within a new-style STAPM simulation. 
Defaults to TRUE.}
}
\value{
Returns a numeric vector of each individual's relative risks for the alcohol related disease specified by "disease".
}
\description{
Computes the relative risks for each alcohol related disease based on the published risk curves.
}
\details{
Relative risks for partially attributable chronic come from published risk functions whose parameters have been
hard-coded within this function rather than being read from an external spreadsheet. For some conditions there are
separate risk functions for morbidity and mortality. For conditions that show a J-shaped risk function that
indicates protective effects of alcohol, there is an option to remove the protective effect by setting all
RR < 1 = 1.   

Relative risks for partially attributable acute are computed by the [tobalcepi::PArisk()] function called from within
 this function. The characteristics of individual single occasion drinking are also calculated within this function 
 using [tobalcepi::AlcBinge_stapm()].     
 
 Relative risks for wholly attributable chronic and wholly attributable acute conditions are calculated
 based on the extent to which either weekly or daily consumption exceeds a pre-specified threshold. The risk 
 for wholly attributable acute conditions is calculated by the function [tobalcepi::WArisk_acute()]. We developed a new 
 method to model the absolute risk of wholly attributable acute conditions to suit the STAPM modelling. 
 This new method is based on the method used to model the risk of partially attributable acute conditions - 
 the shape of the risk function is determined by the individual variation in the total annual number of units that
  are drunk above the male/female thresholds for single occasion binge drinking.
}
\examples{

\dontrun{

# Draw disease specific risk functions

# Example data
data <- data.table(
  GPerDay = 0:100,
  #peakday_grams = 0:100,
  sex = "Female",
  age = 30
)

# Apply the function
test1 <- RRalc(
  data,
  disease = "Pharynx",
  mort_or_morb = "mort"
)

test2 <- RRalc(
  data,
  disease = "Ischaemic_heart_disease",
  mort_or_morb = "morb"
)

test3 <- RRalc(
  data,
  disease = "LiverCirrhosis",
  mort_or_morb = "mort"
)

# Plot the risk functions
plot(test1 ~ I(0:100), type = "l", ylim = c(0, 10), ylab = "rr", 
main = "Females, age 30", xlab = "g per day")
lines(test2 ~ I(0:100), col = 2)
lines(test3 ~ I(0:100), col = 3)
legend("topleft", 
c("Pharyngeal cancer", "Ischaemic heart disease morbidity", "Liver Cirrhosis mortality"), 
lty = 1, col = 1:3)
}
}
