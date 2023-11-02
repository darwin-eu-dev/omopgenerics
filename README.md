
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OMOPGenerics

[![CRANstatus](https://www.r-pkg.org/badges/version/OMOPGenerics)](https://CRAN.R-project.org/package=OMOPGenerics)
[![codecov.io](https://codecov.io/github/darwin-eu/OMOPGenerics/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/OMOPGenerics?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/OMOPGenerics/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/OMOPGenerics/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

The OMOPGenerics package provides definitions of core classes and
methods used by analytic pipelines that query the OMOP common data
model.

## Installation

You can install the development version of OMOPGenerics from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
devtools::install_github("darwin-eu-dev/OMOPGenerics")
```

## Core classes and methods

### CDM Reference

A cdm reference is a single R object that represents OMOP CDM data. The
tables in the cdm reference may be in a database, but a cdm reference
may also contain OMOP CDM tables that are in dataframes/ tibbles or in
arrow. In the latter case the cdm reference would typically be a subset
of an original cdm reference that has been derived as part of a
particular analysis.

OMOPGenerics contains the class definition of a cdm reference and a
dataframe implementation. For creating a cdm reference using a database,
see the CDMConnector package
(<https://darwin-eu.github.io/CDMConnector/>).

``` r
OMOPGenerics::cdmReference
#> function(cdmTables, cdmName, cdmVersion) {
#>   # initial input check
#>   checkInput(cdmTables = cdmTables, cdmName = cdmName, cdmVersion = cdmVersion)
#> 
#>   # constructor
#>   cdm <- newCdmReference(
#>     cdmTables = cdmTables, cdmName = cdmName, cdmVersion = cdmVersion
#>   )
#> 
#>   # validate
#>   cdm <- validateCdmReference(cdm)
#> 
#>   return(cdm)
#> }
#> <bytecode: 0x000001e4c677fb08>
#> <environment: namespace:OMOPGenerics>
```

When the export method is applied to a cdm reference, metadata about
that cdm will be written to a csv. The csv contains the following
columns

| Variable                               | Description |
|----------------------------------------|-------------|
| result_type                            |             |
| cdm_name                               |             |
| cdm_source_name                        |             |
| cdm_description” = “source_description |             |
| cdm_documentation_reference            |             |
| cdm_version                            |             |
| cdm_holder                             |             |
| cdm_release_date                       |             |
| vocabulary_version                     |             |
| person_count                           |             |
| observation_period_count               |             |
| earliest_observation_period_start_date |             |
| latest_observation_period_end_date     |             |
| snapshot_date                          |             |

export method ….

### Concept set

export method ….

### Generatred cohort set

``` r
OMOPGenerics::generatedCohortSet
#> function(cohortTable,
#>                                cohortSetTable = NULL,
#>                                cohortAttritionTable = NULL,
#>                                cohortName = "cohort") {
#>   UseMethod("generatedCohortSet")
#> }
#> <bytecode: 0x000001e4c674a648>
#> <environment: namespace:OMOPGenerics>
```

bind method ….

export method ….

### Summarised result

``` r
OMOPGenerics::summarisedResult
#> function(x) {
#> 
#>   #inital input check
#>   assertTibble(x)
#> 
#>   #constructer
#>   x <- newSummarisedResult(x)
#> 
#> 
#>   # validate
#>   x <- validateSummariseResult(x)
#> 
#> 
#> 
#>   return(x)
#> }
#> <bytecode: 0x000001e4c6d9f838>
#> <environment: namespace:OMOPGenerics>
```

bind method ….

export method ….

### Compared result

bind method ….

export method ….

### Participants
