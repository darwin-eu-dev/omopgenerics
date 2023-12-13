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

#' Summary a cdm reference
#'
#' @param object A cdm reference object.
#' @param ... For compatibility (not used).
#'
#' @export
#'
summary.cdm_reference <- function(object, ...) {
  x <- object
  person_count <- x[["person"]] |> dplyr::tally() |> dplyr::pull("n")
  observation_period_info <- x[["observation_period"]] |>
    dplyr::summarise(
      count = dplyr::n(),
      max = max(.data$observation_period_end_date, na.rm = TRUE),
      min = min(.data$observation_period_start_date, na.rm = TRUE)
    ) |>
    dplyr::collect()
  snapshot_date <- as.character(format(Sys.Date(), "%Y-%m-%d"))

  vocab_version <- getVocabularyVersion(x)

  # get cdm source
  defCdmSource <- dplyr::tibble(
    vocabulary_version = vocab_version,
    cdm_source_name = "",
    cdm_holder = "",
    cdm_release_date = "",
    cdm_version = attr(x, "cdm_version"),
    source_description = "",
    source_documentation_reference = ""
  )
  cdm_source <- tryCatch({
    cdm_source <- x[["cdm_source"]] |>dplyr::collect()
    if (nrow(cdm_source) != 1) {defCdmSource} else {cdm_source}
  },
  error = function(e) {
    defCdmSource
  }
  )

  if (!"vocabulary_version" %in% colnames(cdm_source)) {
    cdm_source <- cdm_source |>
      dplyr::mutate("vocabulary_version" = .env$vocab_version)
  }
  if (!"cdm_version" %in% colnames(cdm_source)) {
    cdm_source <- cdm_source |>
      dplyr::mutate("cdm_version" = attr(x, "cdm_version"))
  }

  cdm_source |>
    dplyr::mutate(
      result_type = "Snapshot",
      cdm_name = dplyr::coalesce(attr(x, "cdm_name"), as.character(NA)),
      vocabulary_version = dplyr::coalesce(
        .env$vocab_version, .data$vocabulary_version
      ),
      person_count = .env$person_count,
      observation_period_count = .env$observation_period_info$count,
      earliest_observation_period_start_date =
        .env$observation_period_info$min,
      latest_observation_period_end_date = .env$observation_period_info$max,
      snapshot_date = .env$snapshot_date
    ) |>
    dplyr::select(
      "result_type",
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
    ) |>
    dplyr::mutate_all(as.character)
}

#' Summary a generated cohort set
#'
#' @param object A generated cohort set object.
#' @param ... For compatibility (not used).
#'
#' @export
#'
summary.generated_cohort_set <- function(object, ...) {
  x <- object
  settings(x) |>
    dplyr::inner_join(attrition(x), by = "cohort_definition_id") |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id) |>
    dplyr::mutate(
      cohort_table_name = attr(x, "tbl_name"),
      cdm_name = cdmName(attr(x, "cdm_reference")),
      result_type = "Cohort details"
    ) |>
    dplyr::relocate(c(
      "result_type", "cdm_name", "cohort_table_name", "cohort_name",
      "cohort_definition_id"
    ))
}
