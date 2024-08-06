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

#' Get cohort counts from a cohort_table object.
#'
#' @param cohort A cohort_table object.
#'
#' @return A table with the counts.
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
#'   observation_period_end_date = as.Date("2023-12-31"),
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
#' cohortCount(cdm$cohort1)
#' }
cohortCount <- function(cohort) {
  assertClass(cohort, "cohort_table")
  if (is.null(attr(cohort, "cohort_attrition"))) {
    cli::cli_abort("Cohort count does not exist for this cohort.")
  }
  x <- attr(cohort, "cohort_attrition") |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x <- x |>
      dplyr::group_by(.data$cohort_definition_id) |>
      dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) |>
      dplyr::ungroup()
  }
  x <- x |>
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) |>
    dplyr::arrange(.data$cohort_definition_id) |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "number_records" = as.integer(.data$number_records),
      "number_subjects" = as.integer(.data$number_subjects)
    )
  return(x)
}
