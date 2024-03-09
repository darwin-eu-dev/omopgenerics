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

#' Drop a table from a cdm object.
#'
#' @param cdm A cdm reference.
#' @param name Name(s) of the table(s) to drop Tidyselect statements are
#' supported.
#'
#' @export
#'
#' @return The cdm reference.
#'
#' @examples
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
#'   )),
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "my_example_cdm",
#'   cohortTables = list("cohort1" = cohort)
#' )
#'
#' cdm
#'
#' cdm <- dropTable(cdm = cdm, name = "cohort1")
#'
#' cdm
#'
dropTable <- function(cdm, name) {
  lifecycle::deprecate_soft(
    when = "0.1.0", what = "dropTable()", with = "dropSourceTable()"
  )
  UseMethod("dropTable")
}

#' @export
dropTable.cdm_reference <- function(cdm, name) {
  dropTable(cdm = cdmSource(cdm), name = name)
  cols <- names(cdm)
  toDrop <- cols |>
    as.list() |>
    rlang::set_names(cols) |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::any_of({{name}})) |>
    colnames()
  if (length(toDrop) > 0) {
    for (nm in toDrop) {
      cdm[[nm]] <- NULL
    }
  }
  return(invisible(cdm))
}

#' @export
dropTable.local_cdm <- function(cdm, name) {
  return(invisible(TRUE))
}
