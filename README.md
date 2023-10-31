
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

## CDM Reference

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
#> <bytecode: 0x000002092a956e58>
#> <environment: namespace:OMOPGenerics>
```

## Concept set

## Generatred cohort set

``` r
OMOPGenerics::generatedCohortSet
#> function(cohortTable,
#>                                cohortSetTable = NULL,
#>                                cohortAttritionTable = NULL,
#>                                cohortName = "cohort") {
#>   UseMethod("generatedCohortSet")
#> }
#> <bytecode: 0x000002092a9029c8>
#> <environment: namespace:OMOPGenerics>
```

## Summarised result

## Summarised result

## Compared result
