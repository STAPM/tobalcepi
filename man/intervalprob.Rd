% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intervalprob.R
\name{intervalprob}
\alias{intervalprob}
\title{Distribution of single-occasion drinking amount}
\usage{
intervalprob(
  grams_ethanol = 1:540,
  SODMean = NULL,
  SODSDV = NULL,
  SODFreq = NULL,
  grams_ethanol_per_unit = 8
)
}
\arguments{
\item{grams_ethanol}{Integer vector - the potential grams of ethanol drunk on a single occasion.}

\item{SODMean}{Numeric vector - the average amount that each individual is expected to
drink on a single drinking occasion.}

\item{SODSDV}{Numeric vector - the standard deviation of the amount that each individual is expected to
drink on a single drinking occasion.}

\item{SODFreq}{Numeric vector - the expected number of drinking occasions that
each individual has each week.}

\item{grams_ethanol_per_unit}{Numeric value giving the conversion factor for the number of grams of pure
ethanol in one UK standard unit of alcohol.}
}
\value{
Returns a numeric vector containing the probability that each amount of alcohol is consumed
}
\description{
Computes the probability that each integer number of grams of ethanol per day 
is consumed on a single drinking occasion.
}
