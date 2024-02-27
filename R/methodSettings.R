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
settings.summarised_result <- function(x) {
  x |>
    dplyr::filter(.data$variable_name == "settings") |>
    dplyr::select("result_id", "cdm_name", "estimate_name", "estimate_value") |>
    tidyr::pivot_wider(
      names_from = "estimate_name", values_from = "estimate_value"
    )
}

