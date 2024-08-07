% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdmFromTables.R
\name{cdmFromTables}
\alias{cdmFromTables}
\title{Create a cdm object from local tables}
\usage{
cdmFromTables(tables, cdmName, cohortTables = list(), cdmVersion = NULL)
}
\arguments{
\item{tables}{List of tables to be part of the cdm object.}

\item{cdmName}{Name of the cdm object.}

\item{cohortTables}{List of tables that contains cohort, cohort_set and
cohort_attrition can be provided as attributes.}

\item{cdmVersion}{Version of the cdm_reference}
}
\value{
A \code{cdm_reference} object.
}
\description{
Create a cdm object from local tables
}
\examples{
\donttest{
library(omopgenerics)
library(dplyr, warn.conflicts = FALSE)

person <- tibble(
  person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
  race_concept_id = 0, ethnicity_concept_id = 0
)
observation_period <- tibble(
  observation_period_id = 1, person_id = 1,
  observation_period_start_date = as.Date("2000-01-01"),
  observation_period_end_date = as.Date("2023-12-31"),
  period_type_concept_id = 0
)
cdm <- cdmFromTables(
  tables = list("person" = person, "observation_period" = observation_period),
  cdmName = "test"
)
}

}
