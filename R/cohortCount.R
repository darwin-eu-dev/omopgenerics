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
cohortCount <- function(cohort) {
  assertClass(cohort, "cohort_table")
  if (is.null(attr(cohort, "cohort_attrition"))) {
    cli::cli_abort("Cohort count does not exist for this cohort.")
  }
  attr(cohort, "cohort_attrition") |>
    dplyr::collect() |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) |>
    dplyr::arrange(.data$cohort_definition_id) |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "number_records" = as.integer(.data$number_records),
      "number_subjects" = as.integer(.data$number_subjects)
    )
}
