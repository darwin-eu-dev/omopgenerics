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

#' Bind two or more objects of the same class.
#'
#' @param ... Objects to bind.
#'
#' @return New object.
#'
#' @export
#'
bind <- function(...) {
  UseMethod("bind")
}

#' Bind cohort_table
#'
#' @param ... Generated cohort set objects to bind. At least two must be
#' provided.
#' @param name Name of the new generated cohort set.
#'
#' @return The cdm object with the new generated cohort set.
#'
#' @export
#'
bind.cohort_table <- function(..., name) {
  # initial checks
  cohorts <- list(...)
  assertList(cohorts, class = "cohort_table")
  if (length(cohorts) < 2) {
    cli::cli_abort(
      "Only {length(cohorts)} cohort provided, at least 2 must be provided."
    )
  }
  assertCharacter(name, length = 1)

  # get cdm
  cdm <- cdmReference(cohorts[[1]])

  # bind
  newCohortSet <- lapply(cohorts, settings) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::mutate("new_cohort_definition_id" = dplyr::row_number())
  repeatedCohortName <- newCohortSet |>
    dplyr::group_by(.data$cohort_name) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::pull("cohort_name") |>
    unique()
  if (length(repeatedCohortName) > 0) {
    repeatedCohort <- lapply(repeatedCohortName, function(x) {
      newCohortSet |>
        dplyr::filter(.data$cohort_name == .env$x) |>
        dplyr::pull("cohort_id") |>
        paste0(collapse = ", ")
    }) |>
      unlist()
    err <- paste0(repeatedCohortName, " in ", repeatedCohort)
    cli::cli_abort("Cohorts can have the same cohort_name: {paste0(err, collapse = '; ')}.")
  }
  newCohortAttrition <- lapply(cohorts, attrition) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::left_join(
      newCohortSet |>
        dplyr::select(
          "cohort_definition_id", "cohort_id", "new_cohort_definition_id"
        ),
      by = c("cohort_definition_id", "cohort_id")
    ) |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_attrition")))
  cohorts <- lapply(seq_len(length(cohorts)), function(x) {
    cohorts[[x]] |>
      dplyr::left_join(
        newCohortSet |>
          dplyr::filter(.data$cohort_id == .env$x) |>
          dplyr::mutate(
            "cohort_definition_id" = as.integer(.data$cohort_definition_id),
            "cohort_name" = as.character(.data$cohort_name)
          ) |>
          dplyr::select("cohort_definition_id", "new_cohort_definition_id"),
        by = c("cohort_definition_id"),
        copy = TRUE
      ) |>
      dplyr::select(-"cohort_definition_id") |>
      dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
  })
  newCohort <- Reduce(dplyr::union_all, cohorts) |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort"))) |>
    dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
  newCohortSet <- newCohortSet |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_set")))

  # instantiate the new generated cohort set
  cdm[[name]] <- newCohortTable(
    table = newCohort,
    cohortSetRef = newCohortSet,
    cohortAttritionRef = newCohortAttrition
  )

  return(cdm)
}
