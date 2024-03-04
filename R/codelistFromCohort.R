
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

#' Get codelist from a cohort_table object.
#'
#' @param cohortTable A cohort_table object.
#' @param cohortId A particular cohort definition id that is present in the
#' cohort table.
#'
#' @return A table with the codelists used.
#'
#' @export
#'
#' @examples
#'  \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2025-12-31"),
#'   period_type_concept_id = 0
#' )
#' cohort <- tibble(
#'   cohort_definition_id = c(1, 1, 1, 2),
#'   subject_id = 1,
#'   cohort_start_date = as.Date(c(
#'     "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
#'   )),
#'   cohort_end_date = as.Date(c(
#'     "2020-01-01", "2021-01-01", "2022-01-01", "2022-01-01"
#'   ))
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "my_example_cdm",
#'   cohortTables = list("cohort1" = cohort)
#' )
#' cdm$cohort1 <- newCohortTable(table = cdm$cohort1,
#'                               codelistRef = dplyr::tibble(
#'                                 cohort_definition_id = c(1,1,1,2,2),
#'                                 codelist_name =c("disease X", "disease X", "disease X",
#'                                                  "disease Y", "disease Y"),
#'                                 concept_id = c(1,2,3,4,5)
#'                               ))
#' codelistFromCohort(cdm$cohort1)
#' }
codelistFromCohort <- function(cohortTable,
                               cohortId = NULL) {
  assertClass(cohortTable, "cohort_table")
  if(is.null(attr(cohortTable, "cohort_codelist"))){
    cli::cli_abort("Codelist does not exist for this cohort.")
  }

  if(isFALSE(all(cohortColumns("cohort_codelist") %in%
  colnames(attr(cohortTable, "cohort_codelist"))))){
    cli::cli_abort("Codelist does not have the expected columns: {cohortColumns('cohort_codelist')}")
  }

  if(!is.null(cohortId) &&
     nrow(settings(cohortTable) |>
           dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)) == 0){
    cli::cli_abort("Codelist does not exist for the specified cohorts")
  }

  x <- attr(cohortTable, "cohort_codelist")  |>
    dplyr::collect()

  if(!is.null(cohortId)){
    x <- x |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  }



  x <-  studySpecs <- split(
    x,
    x[, c("codelist_name")],
    drop = TRUE
  )
  for(i in seq_along(x)){
    x[[i]] <- x[[i]] |>
      dplyr::pull("concept_id")
  }
  x <- newCodelist(x)

  return(x)
}
