# Copyright 2023 DARWIN EU (C)
#
# This file is part of OMOPGenerics
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

#' Bind generated_cohort_set
#'
#' @param ... Generated cohort set objects to bind. At least two must be
#' provided.
#' @param name Name of the new generated cohort set.
#'
#' @return The cdm object with the new generated cohort set.
#'
#' @export
#'
bind.generated_cohort_set <- function(..., name) {
  # initial checks
  cohorts <- list(...)
  assertList(cohorts, class = "generated_cohort_set")
  if (length(cohorts) < 2) {
    cli::cli_abort(
      "Only {length(cohorts)} cohort provided, at least 2 must be provided."
    )
  }
  assertCharacter(name, length = 1)

  # bind
  newCohortSet <- lapply(cohorts, settings) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::mutate("new_cohort_definition_id" = dplyr::row_number())
  newCohortAttrition <- lapply(cohorts, attrition) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::left_join(newCohortSet, by = c("cohort_definition_id", "cohort_id")) |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_attrition")))
  newCohort <- Reduce(cohorts, dplyr::union_all) |>
    dplyr::left_join(
      newCohortSet |>
        dplyr::select("cohort_definition_id", "new_cohort_definition_id"),
      by = "cohort_definition_id",
      copy = TRUE
    ) |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort"))) |>
    dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
  newCohortSet <- newCohortSet |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_set")))

  # instantiate the new generated cohort set
  cdm <- generatedCohortSet(
    cohortRef = newCohort,
    cohortSetRef = newCohortSet,
    cohortAttritionRef = newCohortAttrition
  )

  return(cdm)
}
