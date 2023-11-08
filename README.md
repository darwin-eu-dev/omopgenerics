
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

### Concept set

### Generatred cohort set

### Summarised result

### Compared result

### Participants
