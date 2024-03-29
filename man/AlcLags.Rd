% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AlcLags.R
\name{AlcLags}
\alias{AlcLags}
\title{Alcohol lag times}
\usage{
AlcLags(disease_name = c("Pharynx", "Oral_cavity"), n_years = 20)
}
\arguments{
\item{disease_name}{Character - the name of the disease under consideration.}

\item{n_years}{Integer - the number of years from 1 to n over which the effect of a change in
consumption emerges. Defaults to 20 years to fit with the current lag data.}
}
\value{
Returns a data table with two columns - one for the years since consumption changed, and the other
that gives the proportion by which the effect of a change in consumption
on an individual's relative risk of disease has so far emerged.
}
\description{
Prepare the disease specific functions that describe how a change in alcohol consumption
gradually has an effect on the relative risk of disease incidence over time (up to 20 years)
since alcohol consumption changed.
}
\details{
All lag times are taken from the review by \insertCite{holmes2012temporal;textual}{tobalcepi} ,
 and are the numbers used in the current version of SAPM.
}
\examples{
\dontrun{
AlcLags("Pharynx")
}
}
\references{
\insertRef{holmes2012temporal}{tobalcepi}
}
