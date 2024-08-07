% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classCdmReference.R
\name{[[.cdm_reference}
\alias{[[.cdm_reference}
\title{Subset a cdm reference object.}
\usage{
\method{[[}{cdm_reference}(x, name)
}
\arguments{
\item{x}{A cdm reference}

\item{name}{The name or index of the table to extract from the cdm object.}
}
\value{
A single cdm table reference
}
\description{
Subset a cdm reference object.
}
\examples{
\donttest{
library(omopgenerics)
library(dplyr, warn.conflicts = FALSE)

cdm <- cdmFromTables(
  tables = list(
    "person" = tibble(
      person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
      race_concept_id = 0, ethnicity_concept_id = 0
    ),
    "observation_period" = tibble(
      observation_period_id = 1:3, person_id = 1:3,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2023-12-31"),
      period_type_concept_id = 0
    )
  ),
  cdmName = "mock"
)

cdm[["person"]]
}
}
