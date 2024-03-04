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

#' Get settings from an object.
#'
#' @param x Object
#'
#' @return A table with the settings of the object.
#'
#' @export
settings <- function(x) {
  UseMethod("settings")
}

#' Get cohort settings from a cohort_table object.
#'
#' @param x A cohort_table object.
#'
#' @return A table with the details of the cohort settings.
#'
#' @export
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
#'   cohort_definition_id = 1,
#'   subject_id = 1,
#'   cohort_start_date = as.Date("2010-01-01"),
#'   cohort_end_date = as.Date("2012-01-01")
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test",
#'   cohortTables = list("my_cohort" = cohort)
#' )
#'
#' settings(cdm$my_cohort)
#'
#' cdm$my_cohort <- cdm$my_cohort |>
#'   newCohortTable(cohortSetRef = tibble(
#'     cohort_definition_id = 1, cohort_name = "new_name"
#'   ))
#'
#' settings(cdm$my_cohort)
#'
settings.cohort_table <- function(x) {
  if (is.null(attr(x, "cohort_set"))) {
    cli::cli_abort("Cohort settings does not exist for this cohort.")
  }
  attr(x, "cohort_set") |>
    dplyr::collect() |>
    dplyr::relocate(c("cohort_definition_id", "cohort_name")) |>
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get settings from a summarised_result object.
#'
#' @param x A summarised_result object.
#'
#' @return A table with the settings.
#'
#' @export
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
#'   cohort_definition_id = 1,
#'   subject_id = 1,
#'   cohort_start_date = as.Date("2010-01-01"),
#'   cohort_end_date = as.Date("2012-01-01")
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test",
#'   cohortTables = list("my_cohort" = cohort)
#' )
#'
#' result <- summary(cdm$my_cohort)
#'
#' settings(result)
#'
settings.summarised_result <- function(x) {
  settingsRaw <- x |>
    dplyr::filter(.data$variable_name == "settings") |>
    dplyr::select(
      "result_id", "cdm_name", "result_type", "estimate_name", "estimate_type",
      "estimate_value"
    )
  set <- settingsRaw |>
    dplyr::select(-"estimate_type") |>
    tidyr::pivot_wider(
      names_from = "estimate_name", values_from = "estimate_value"
    )
  cols <- settingsRaw |> dplyr::pull("estimate_name") |> unique()
  for (col in cols) {
    type <- settingsRaw |>
      dplyr::filter(.data$estimate_name == .env$col) |>
      dplyr::pull("estimate_type") |>
      unique()
    if (length(type == 1)) {
      set <- set |>
        dplyr::mutate(!!col := giveType(.data[[col]], type))
    }
  }
  return(set)
}

