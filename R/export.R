# Copyright 2023 DARWIN EU (C)
#
# This file is part of CDMUtilities
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

#' Export an object.
#'
#' @param ... Object to export.
#'
#' @return Exported object in tbl format.
#'
#' @export
#'
export <- function(...) {
  UseMethod("export")
}

#' Export a GeneratedCohortSet.
#'
#' @param cohort A generatedCohortSet object.
#'
#' @return Exported generatedCohortSet.
#'
#' @export
#'
export.GeneratedCohortSet <- function(cohort) {
  cohortSet(cohort) %>%
    dplyr::inner_join(cohortAttrition(cohort), by = "cohort_definition_id")
}

#' Export a cdm_reference
#'
#' @param cdm A cdm_reference object.
#'
#' @return Exported cdm_reference.
#'
#' @export
#'
export.cdm_reference <- function(cdm) {
  person_count <- cdm[["person"]] %>% dplyr::tally() %>% dplyr::pull("n")
  observation_period_count <- cdm[["observation_period"]] %>%
    dplyr::tally() %>%
    dplyr::pull("n")
  observation_period_range <- cdm[["observation_period"]] %>%
    dplyr::summarise(
      max = max(.data$observation_period_end_date, na.rm = TRUE),
      min = min(.data$observation_period_start_date, na.rm = TRUE)
    ) %>%
    dplyr::collect()
  snapshot_date <- as.character(format(Sys.Date(), "%Y-%m-%d"))

  vocab_version <- cdm[["vocabulary"]] %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::pull(.data$vocabulary_version)

  if (length(vocab_version) == 0) {
    vocab_version <- NA_character_
  }

  cdm_source_name <- cdm$cdm_source %>% dplyr::pull("cdm_source_name")

  cdm_source <- cdm[["cdm_source"]] %>% dplyr::collect()
  if (nrow(cdm_source) == 0) {
    cdm_source <- dplyr::tibble(
      vocabulary_version = vocab_version,
      cdm_source_name = "",
      cdm_holder = "",
      cdm_release_date = "",
      cdm_version = attr(cdm, "cdm_version"),
      source_description = "",
      source_documentation_reference = ""
    )
  }

  cdm_source %>%
    dplyr::mutate(
      cdm_name = dplyr::coalesce(attr(cdm, "cdm_name"), as.character(NA)),
      vocabulary_version = dplyr::coalesce(
        .env$vocab_version, .data$vocabulary_version
      ),
      person_count = .env$person_count,
      observation_period_count = .env$observation_period_count,
      earliest_observation_period_start_date =
        .env$observation_period_range$min,
      latest_observation_period_end_date = .env$observation_period_range$max,
      snapshot_date = .env$snapshot_date
    ) %>%
    dplyr::select(
      "cdm_name",
      "cdm_source_name",
      "cdm_description" = "source_description",
      "cdm_documentation_reference" = "source_documentation_reference",
      "cdm_version",
      "cdm_holder",
      "cdm_release_date",
      "vocabulary_version",
      "person_count",
      "observation_period_count",
      "earliest_observation_period_start_date",
      "latest_observation_period_end_date",
      "snapshot_date"
    ) %>%
    dplyr::mutate_all(as.character)
}
