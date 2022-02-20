# wrappedgithubactivityapi

[![R-CI-check](https://github.com/AndrewNg1891/DATA534_API_Project_Group9/actions/workflows/R-CI-check.yaml/badge.svg?branch=CI_testing)](https://github.com/AndrewNg1891/DATA534_API_Project_Group9/actions/workflows/R-CI-check.yaml)

## Overview

The `wrappedgithubactivityapi` package wrapped a part of the [GitHub Activity APIs](https://docs.github.com/en/rest/reference/activity) for helping user to retrieve public / repository network / organization / repository events (activities). The package also provided some analysis conveniences by its statistical functionality and plot functionality.

## Installation Instruction

To install the package on Github, please use the following code :

    install.packages("devtools")
    devtools::install_github("AndrewNg1891/DATA534_API_Project_Group9")

To import the package in the R code, please use the following code after installation :

    library(wrappedgithubactivityapi)


To install the package via [CRAN](https://cran.r-project.org/), please the following commands :

    install.packages("wrappedgithubactivityapi")
    library(wrappedgithubactivityapi)

## Function List

`github_get_public_events()`

The function will retrieve public events data and return the data in user selected format.

`github_get_organization_events()`

The function will retrieve organization events data and return the data in user selected format.

`github_get_network_events()`

The function will retrieve repository network events data and return the data in user selected format.

`github_get_repo_events()`

The function will retrieve repository events data and return the data in user selected format.

`github_lastn_events_df()`

The function will retrieve last n events data of a certain range (Possible value: "all public"/"network"/"organization"/"repository") and return the dataframe that contains the retrieved events data.of a certain range . The information in the response was selected on 5 most important things. 

`github_count_events_bytype()`

The function will count the number of events by type for a certain range.

`github_count_events_bydate()`

The function will count the number of events by date for a certain range and type.

`github_count_events_byweekday()`

The function will count the number of events by weekdays for a certain range and type.

`api_rate_limit()`

The function can be used to get rate limit information

## Usage Example

    github_lastn_events_df("repository",15,owner="AndrewNg1891",repo="DATA534_API_Project_Group9")

For more usage example, please see `\vignettes\Introduction.Rmd`
