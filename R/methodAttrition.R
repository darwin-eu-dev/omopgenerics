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

#' Get attrition from an object.
#'
#' @param x x.
#'
#' @return A table with the attrition.
#'
#' @export
attrition <- function(x) {
  UseMethod("attrition")
}

#' Get cohort attrition from a cohort_table object.
#'
#' @param x A cohort_table object.
#'
#' @return A table with the attrition.
#'
#' @export
attrition.cohort_table <- function(x) {
  if (is.null(attr(x, "cohort_attrition"))) {
    cli::cli_abort("Cohort attrition does not exist for this cohort.")
  }
  x <- attr(x, "cohort_attrition") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id) |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "number_records" = as.integer(.data$number_records),
      "number_subjects" = as.integer(.data$number_subjects),
      "reason_id" = as.integer(.data$reason_id),
      "reason" = as.character(.data$reason),
      "excluded_records" = as.integer(.data$excluded_records),
      "excluded_subjects" = as.integer(.data$excluded_subjects)
    )
  x <- addClass(x, c("omop_attrition", "attrition"))
  return(x)
}
