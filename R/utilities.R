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

addClass <- function(x, value) {
  if (any(value %in% class(x))) x <- removeClass(x, value)
  base::class(x) <- c(value, base::class(x))
  return(x)
}
removeClass <- function(x, value) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
getVocabularyVersion <- function(x) {
  vocabVersion <- NULL
  if ("vocabulary" %in% names(x) && "vocabulary_version" %in% colnames(x)) {
    vocabVersion <- x[["vocabulary"]] |>
      dplyr::filter(.data$vocabulary_id == "None") |>
      dplyr::pull(.data$vocabulary_version)
  }
  if (length(vocabVersion) == 0) {
    vocabVersion <- NA_character_
  }
  return(vocabVersion)
}

#' Get the cohort definition id of a certain name
#'
#' @param cohort A cohort_table object.
#' @param cohortName Names of the cohort of interest.
#'
#' @return Cohort definition ids
#'
#' @export
#'
getCohortId <- function(cohort, cohortName) {
  # check inputs
  assertClass(cohort, "cohort_table")
  assertCharacter(cohortName)

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")
  notPresent <- cohortName[!cohortName %in% set$cohort_name]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts names not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  x <- dplyr::tibble("cohort_name" = cohortName) |>
    dplyr::inner_join(set, by = "cohort_name")
  x$cohort_definition_id |> rlang::set_names(x$cohort_name)
}

#' Get the cohort name of a certain cohort definition id
#'
#' @param cohort A cohort_table object.
#' @param cohortId Cohort deifnition id of interest.
#'
#' @return Cohort names
#'
#' @export
#'
getCohortName <- function(cohort, cohortId) {
  # check inputs
  assertClass(cohort, "cohort_table")
  assertNumeric(cohortId, integerish = TRUE)

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")
  notPresent <- cohortId[!cohortId %in% set$cohort_definition_id]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts definition ids not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  x <- dplyr::tibble("cohort_definition_id" = as.integer(cohortId)) |>
    dplyr::inner_join(set, by = "cohort_definition_id")
  x$cohort_name |> rlang::set_names(x$cohort_definition_id)
}
