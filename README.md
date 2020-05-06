
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tobalcepi <img src="tools/tobalcepi_hex.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)  
[![DOI](https://zenodo.org/badge/221235909.svg)](https://zenodo.org/badge/latestdoi/221235909)

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

This work builds on the [disease lists and risk functions collated
during the process of developing the Sheffield Alcohol Policy
Model](https://figshare.shef.ac.uk/articles/Alcohol-attributable_diseases_and_dose-response_curves_for_the_Sheffield_Alcohol_Policy_Model_version_4_0/6819689/2)
(Angus et al. [2018](#ref-Angus2018)). For tobacco, we based our inital
disease list and risk functions on those collated for the Royal College
of Physician’s report “Hiding in plain sight: Treating tobacco
dependency in the NHS” (Tobacco Advisory Group of the Royal College of
Physicians [2018](#ref-RCP2018)). We then went through a process of
checking and harmonising the tobacco disease list and risk functions
against our work for alcohol (Angus et al. [2018](#ref-Angus2018)) and
work by Cancer Research UK (Brown et al. [2018](#ref-Brown2018)). We
have documented the decisions we made in this initial process in a
[short
report](https://figshare.com/articles/Smoking_and_the_risks_of_adult_diseases/7411451)
(Webster et al. [2018](#ref-Webster2018)). Subsequent updates to the
tobacco related risk functions are documented in
`vignette("smoking-disease-risks")`.

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

We would like to ask that since the code and documentation is still
under development and is complex, that you consult with the authors
before you use it.

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster, Maddy Henney, Colin Angus and Alan
Brennan (2020). tobalcepi: Risk Functions and Attributable Fractions for
Tobacco and Alcohol. R package version x.x.x.
<https://STAPM.github.io/tobalcepi/>. DOI:”

-----

Since you will be downloading and installing a source package, you might
need to set your system up for building R packages:

It is a good idea to update R and all of your packages.

**Mac OS**: A convenient way to get the tools needed for compilation is
to install Xcode Command Line Tools. Note that this is much smaller than
full Xcode. In a shell, enter xcode-select –install. For installing
almost anything else, consider using [Homebrew](https://brew.sh/).

**Windows**: Install Rtools. This is not an R package\! It is “a
collection of resources for building packages for R under Microsoft
Windows, or for building R itself”. Go to
<https://cran.r-project.org/bin/windows/Rtools/> and install as
instructed.

-----

You can **install the development version of `hseclean`** from github
with:

``` r
#install.packages("devtools")
devtools::install_github("STAPM/tobalcepi")
```

-----

If there is an error with `install_github()`, one possible work-around
is

1.  Download the package “tarball” by copying this into your internet
    browser (making sure the numbers at the end indicate the latest
    version) `https://github.com/STAPM/tobalcepi/tarball/1.0.0`. When
    the window pops up, choose where to save the .tar.gz file.

2.  Go to the Terminal window in R Studio (or a console window in
    Windows by searching for “cmd”) and install the package from the
    downloaded file by typing `R CMD INSTALL file_path.tar.gz`.

-----

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

## Getting started

*to be added*

## Basic functionality

*to be added*

## Acknowledgements

This work builds on the years of work and experience of the Sheffield
Alcohol Research Group in constructing the Sheffield Alcohol Policy
Model. The extension of the work for alcohol was initially extended to
tobacco as part of our development of the Sheffield Tobacco and Alcohol
Policy Model as part of the UK Centre for Tobacco and Alcohol Studies
(<http://ukctas.net/>).

-----

<div id="refs" class="references">

<div id="ref-Angus2018">

Angus, Colin, M Henney, L Webster, and Duncan Gillespie. 2018.
“Alcohol-Attributable Diseases and Dose-Response Curves for the
Sheffield Alcohol Policy Model Version 4.0.”
<https://doi.org/10.15131/shef.data.6819689.v1>.

</div>

<div id="ref-Brown2018">

Brown, Katrina F., Harriet Rumgay, Casey Dunlop, Margaret Ryan, Frances
Quartly, Alison Cox, Andrew Deas, et al. 2018. “The Fraction of Cancer
Attributable to Modifiable Risk Factors in England, Wales, Scotland,
Northern Ireland, and the United Kingdom in 2015.” Journal Article.
*British Journal of Cancer* 118 (8): 1130–41.
<https://doi.org/10.1038/s41416-018-0029-6>.

</div>

<div id="ref-RCP2018">

Tobacco Advisory Group of the Royal College of Physicians. 2018. “Hiding
in plain sight: Treating tobacco dependency in the NHS.” Research
report. *Available from:
Https://Www.rcplondon.ac.uk/Projects/Outputs/Hiding-Plain-Sight-Treating-Tobacco-Dependency-Nhs*.

</div>

<div id="ref-Webster2018">

Webster, L, C Angus, A Brennan, and D. Gillespie. 2018. “Smoking and the
Risks of Adult Diseases.”

</div>

</div>
