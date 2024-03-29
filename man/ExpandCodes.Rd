% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExpandCodes.R
\name{ExpandCodes}
\alias{ExpandCodes}
\title{Convert groups of ICD-10 codes to single codes}
\usage{
ExpandCodes(lkup)
}
\arguments{
\item{lkup}{Data frame containing the disease list.}
}
\value{
Returns a data frame containing a row for each single ICD-10 code to be searched for.
}
\description{
Creates the lookup files for search for single ICD-10 codes related to tobacco and/or alcohol.
}
\details{
For example, if one disease category is C00-C06 (oral cancer), this includes the single codes
C00,C01,C02,C03,C04,C05,C06. The number of rows will be expanded to give each single code
its own row.
}
\examples{

\dontrun{

ExpandCodes(lkup)

}

}
