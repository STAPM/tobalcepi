
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tobalcepi <img src="tools/tobalcepi_hex.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

**DRAFT WORKING VERSION** - The package is usable but there are still
bugs and further developments that are being worked through i.e. some
code and documentation is still incomplete or in need of being refined.
The code and documentation are still undergoing internal review by the
analyst team.

## Motivation

`tobalcepi` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

The motivation for `tobalcepi` was to organise the information on the
relative risks of diseases in adults related to their own tobacco and
alcohol consumption and to provide functions to easily work with these
data in modelling. The suite of functions within `tobalcepi` processes
the published data on disease risks that stem from chronic and acute
alcohol consumption, from smoking, and on the decline in risk after
ceasing or reducing consumption. The package also includes functions to
estimate population attributable fractions, and to explore the
interaction between the disease risks that stem from tobacco and alcohol
consumption.

> The disease lists and risk functions in this package all have
> published sources, which we have referenced. In order to obtain
> mathematical descriptions of the risk functions for use in modelling,
> we needed to contact some authors to ask for addition information.

## Usage

`tobalcepi` is a package for predicting individual risk of disease due
to tobacco and alcohol consumption based on published sources, and
summarising that risk.

The **inputs** are the published estimates of relative risk for each
disease (sometimes stratified by population subgroup).

The **processes** applied by the functions in `tobalcepi` give options
to estimate:

1.  The risk of injury or disease from acute alcohol consumption.  
2.  The risk of chronic disease based on the current amount of alcohol
    consumed.  
3.  The risk of chronic disease based on whether someone currently
    smokes, and how much they currently smoke.  
4.  The combined risk of disease in someone who smokes and drinks.  
5.  The change in risk of disease after someone ceases or reduces their
    consumption.  
6.  The population attributable fractions of disease to tobacco and/or
    alcohol (given suitable data on tobacco and alcohol consumption).

The **outputs** of these processes are datasets in which an individual’s
tobacco and/or alcohol consumption has been matched to their relative
risks of certain diseases, and aggregated datasets that summarise the
risks of disease within certain population subgroups.

## Installation

`tobalcepi` is currently available only to members of the project team
(but please contact Duncan Gillespie <duncan.gillespie@sheffield.ac.uk>
to discuss). To access you need to [**sign-up for a GitLab
account**](https://gitlab.com/). You will then need to be added to the
STAPM project team to gain access.

Once that is sorted, you can **install the development version** from
GitLab with:

``` r
#install.packages("devtools")

devtools::install_git(
  "https://gitlab.com/stapm/tobalcepi.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass())
)

# Where uname is your Gitlab user name.
# this should make a box pop up where you enter your GitLab password
```

Then load the package, and some other packages that are useful. Note
that the code within `tobalcepi` uses the `data.table::data.table()`
syntax.

``` r
# Load the package
library(tobalcepi)

# Other useful packages
library(dplyr) # for data manipulation and summary
library(magrittr) # for pipes
library(ggplot2) # for plotting
```

## Citation

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster, Maddy Henney, Colin Angus and Alan
Brennan (2020). tobalcepi: Risk Functions and Attributable Fractions for
Tobacco and Alcohol. R package version x.x.x.
<https://stapm.gitlab.io/tobalcepi>”
