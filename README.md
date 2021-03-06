
<!-- README.md is generated from README.Rmd. Please edit README.Rmd (this file) -->

# Globant test

[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![Travis-CI Build
Status](https://travis-ci.org/bcgov/bcgovr.svg?branch=master)](https://travis-ci.org/bcgov/bcgovr)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

This is the repository associated with the data science knowledge test.
This is not a package generation attempt, but collects all the
information needed to reproduce the test such as package, functions and
code.

The
[dataset](https://data.birmingham.gov.uk/dataset/purchase-card-transactions)
to develop the test is a collection of purchase card transactions for
the Birmingham City Council.

    ├── cleaning_data.R -> 01
    ├── merge_data.R -> 02
    ├── descriptive_analysis.R -> 03

The cleaning\_data.R script uses the
[rvest](https://github.com/tidyverse/rvest) package for web scraping
development. To standardize the process it is necessary to run both
cleaning\_data.R and merge\_data.R. In the repository you will find the
file that was used in rmarkdown, called document.Rmd. For the
development of the model, we worked with the
[tidymodels](https://www.tidymodels.org) environment.

It is recommended to use a version of [R](https://cran.dcc.uchile.cl)
higher than 4.0 and to install the latest version for each package used.
It is possible to use [pacman](https://github.com/trinker/pacman) for
ease of use.
