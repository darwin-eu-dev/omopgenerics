% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classOmopTable.R
\name{emptyOmopTable}
\alias{emptyOmopTable}
\title{Create an empty omop table}
\usage{
emptyOmopTable(cdm, name)
}
\arguments{
\item{cdm}{A cdm_reference to create the table.}

\item{name}{Name of the table to create.}
}
\value{
The cdm_reference with an empty cohort table
}
\description{
Create an empty omop table
}
\examples{
library(omopgenerics)

person <- dplyr::tibble(
  person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
  race_concept_id = 0, ethnicity_concept_id = 0
)
observation_period <- dplyr::tibble(
  observation_period_id = 1, person_id = 1,
  observation_period_start_date = as.Date("2000-01-01"),
  observation_period_end_date = as.Date("2023-12-31"),
  period_type_concept_id = 0
)
cdm <- cdmFromTables(
  tables = list("person" = person, "observation_period" = observation_period),
  cdmName = "test"
)

cdm <- emptyOmopTable(cdm, "drug_exposure")

cdm$drug_exposure

}
