# Copyright 2023 DARWIN EU (C)
#
# This file is part of omopgenerics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create a cdm object from local tables
#'
#' @param tables List of tables to be part of the cdm object.
#' @param cdmName Name of the cdm object.
#' @param cohortTables List of tables that contains cohort, cohort_set and
#' cohort_attrition can be provided as attributes.
#' @param cdmVersion Version of the cdm_reference
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
#' @examples
#' \donttest{
#'person <- dplyr::tibble(
#'  person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'  race_concept_id = 0, ethnicity_concept_id = 0
#')
#'observation_period <- dplyr::tibble(
#'  observation_period_id = 1, person_id = 1,
#'  observation_period_start_date = as.Date("2000-01-01"),
#'  observation_period_end_date = as.Date("2025-12-31"),
#'  period_type_concept_id = 0
#')
#'cdm <- cdmFromTables(
#'  tables = list("person" = person, "observation_period" = observation_period),
#'  cdmName = "test"
#')
#'}
#'
cdmFromTables <- function(tables,
                          cdmName,
                          cohortTables = list(),
                          cdmVersion = NULL) {
  # check input
  assertList(tables, named = TRUE, class = "data.frame")
  assertList(cohortTables, named = TRUE, class = "data.frame")
  if (missing(cdmName)) {
    if ("cdm_source" %in% names(tables)) {
      cdmName <- tables[["cdm_source"]] |> dplyr::pull("cdm_source_name")
      cli::cli_warn("cdmName not provided, obtained from cdm_source table.")
    } else {
      cli::cli_abort("cdmName not provided")
    }
  }
  assertCharacter(cdmName, length = 1)
  assertCharacter(cdmVersion, length = 1, null = T)

  src <- newLocalSource()
  for (nm in names(tables)) {
    tables[[nm]] <- tables[[nm]] |>
      dplyr::as_tibble() |>
      newCdmTable(src = src, name = nm)
  }
  cdm <- newCdmReference(
    tables = tables, cdmName = cdmName, cdmVersion = cdmVersion
  )

  for (nm in names(cohortTables)) {
    cdm <- insertTable(cdm = cdm, name = nm, table = cohortTables[[nm]])
    cdm[[nm]] <- cdm[[nm]] |>
      newCohortTable(
        cohortSetRef = attr(cohortTables[[nm]], "cohort_set"),
        cohortAttritionRef = attr(cohortTables[[nm]], "cohort_attrition")
      )
  }

  return(cdm)
}

newLocalSource <- function() {
  structure(.Data = list(), class = "local_cdm") |>
    newCdmSource(sourceType = "local")
}

