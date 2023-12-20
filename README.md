
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omopgenerics

[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

The omopgenerics package provides definitions of core classes and
methods used by analytic pipelines that query the OMOP common data
model.

## Installation

You can install the development version of OMOPGenerics from [GitHub](https://github.com/) with:

``` r
install.packages("remotes")
devtools::install_github("darwin-eu-dev/omopgenerics")
```

And load it using the library command:

``` r
library(omopgenerics)
#> 
#> Attaching package: 'omopgenerics'
#> The following object is masked from 'package:stats':
#> 
#>     aggregate
```

## Core classes and methods

### CDM Reference

A cdm reference is a single R object that represents OMOP CDM data. The
tables in the cdm reference may be in a database, but a cdm reference
may also contain OMOP CDM tables that are in dataframes/tibbles or in
arrow. In the latter case the cdm reference would typically be a subset
of an original cdm reference that has been derived as part of a
particular analysis.

omopgenerics contains the class definition of a cdm reference and a
dataframe implementation. For creating a cdm reference using a database,
see the CDMConnector package
(<https://darwin-eu.github.io/CDMConnector/>).

A cdm object can contain four type of tables:

- Standard tables:

``` r
omopTables()
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
omopColumns(table = "person")
#> [1] "person_id"            "gender_concept_id"    "year_of_birth"       
#> [4] "race_concept_id"      "ethnicity_concept_id"
```

- Cohort tables We can see the cohort-related tables and their required
  columns.

``` r
cohortTables()
#> [1] "cohort"           "cohort_set"       "cohort_attrition"
cohortColumns(table = "cohort")
#> [1] "cohort_definition_id" "subject_id"           "cohort_start_date"   
#> [4] "cohort_end_date"
```

In addition, cohorts are defined in terms of a `generatedCohortSet`
class. For more details on this class definition see the corresponding
vignette.

- Achilles tables The Achilles R package generates descriptive
  statistics about the data contained in the OMOP CDM. Again, we can see
  the tables created and their required columns.

``` r
achillesTables()
#> [1] "achilles_analysis"     "achilles_results"      "achilles_results_dist"
achillesColumns(table = "achilles_results")
#> [1] "analysis_id" "stratum_1"   "stratum_2"   "stratum_3"   "stratum_4"  
#> [6] "stratum_5"   "count_value"
```

- Other tables, these other tables can have any format.

Any table to be part of a cdm object has to fulfill 4 conditions:

- All must share a common source.

- The name of the tables must be lowercase.

- The name of the column names of each table must be lowercase.

- `person` and `observation_period` must be present.

### Concept set

A concept set can be represented as either a codelist or a concept set
expression. A codelist is a named list, with each item of the list
containing specific concept IDs.

``` r
condition_codes <- list("diabetes" = c(201820, 4087682, 3655269),
                        "asthma" = 317009)
condition_codes <- codelist(condition_codes)

condition_codes
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - diabetes (3 codes)
#> - asthma (1 codes)
```

Meanwhile, a concept set expression provides a high-level definition of
concepts that, when applied to a specific OMOP CDM vocabulary version
(by making use of the concept hierarchies and relationships), will
result in a codelist.

``` r
condition_cs <- list(
  "diabetes" = dplyr::tibble(
    "concept_id" = c(201820, 4087682),
    "excluded" = c(FALSE, FALSE),
    "descendants" = c(TRUE, FALSE),
    "mapped" = c(FALSE, FALSE)
  ),
  "asthma" = dplyr::tibble(
    "concept_id" = 317009,
    "excluded" = FALSE,
    "descendants" = FALSE,
    "mapped" = FALSE
  )
)
condition_cs <- conceptSetExpression(condition_cs)

condition_cs
#> 
#> ── 2 conceptSetExpressions ─────────────────────────────────────────────────────
#> 
#> - diabetes (2 concept criteria)
#> - asthma (1 concept criteria)
```

### Summarised result

### Compared result
