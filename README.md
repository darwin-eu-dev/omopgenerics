
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

You can install the CRAN version of the
[OMOPGenerics](https://CRAN.R-project.org/package=OMOPGenerics) from
CRAN:

``` r
install.packages("OMOPGenerics")
```

You can install the development version of OMOPGenerics from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
devtools::install_github("darwin-eu-dev/OMOPGenerics")
```

And load it using the library command:

``` r
library(OMOPGenerics)
```

## Core classes and methods

### CDM Reference

A cdm reference is a single R object that represents OMOP CDM data. The
tables in the cdm reference may be in a database, but a cdm reference
may also contain OMOP CDM tables that are in dataframes/tibbles or in
arrow. In the latter case the cdm reference would typically be a subset
of an original cdm reference that has been derived as part of a
particular analysis.

OMOPGenerics contains the class definition of a cdm reference and a
dataframe implementation. For creating a cdm reference using a database,
see the CDMConnector package
(<https://darwin-eu.github.io/CDMConnector/>).

A cdm object can contain three type of tables:

- Standard tables:

``` r
standardOmopCdmTables()
#>  [1] "person"                "observation_period"    "visit_occurrence"     
#>  [4] "visit_detail"          "condition_occurrence"  "drug_exposure"        
#>  [7] "procedure_occurrence"  "device_exposure"       "measurement"          
#> [10] "observation"           "death"                 "note"                 
#> [13] "note_nlp"              "specimen"              "fact_relationship"    
#> [16] "location"              "care_site"             "provider"             
#> [19] "payer_plan_period"     "cost"                  "drug_era"             
#> [22] "dose_era"              "condition_era"         "metadata"             
#> [25] "cdm_source"            "concept"               "vocabulary"           
#> [28] "domain"                "concept_class"         "concept_relationship" 
#> [31] "relationship"          "concept_synonym"       "concept_ancestor"     
#> [34] "source_to_concept_map" "drug_strength"         "cohort_definition"    
#> [37] "attribute_definition"
```

Each one of the tables has a required columns. For example, for the
`person` table this are the required columns:

``` r
requiredOmopCdmColumns(table = "person")
#> [1] "person_id"            "gender_concept_id"    "year_of_birth"       
#> [4] "race_concept_id"      "ethnicity_concept_id"
```

- Cohort tables (see `generatedCohortSet`).

- Other tables, these other tables can have any format.

Any table to be part of a cdm object has to fulfill 4 conditions:

- All must share a common source.

- The name of the tables must be lowercase.

- The name of the column names of each table must be lowercase.

- `person` and `observation_period` must be present.

### Concept set

### Generatred cohort set

### Summarised result

### Compared result

### Participants
