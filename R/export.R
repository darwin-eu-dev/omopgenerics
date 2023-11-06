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

#' Export an object.
#'
#' @param x Object to export.
#' @param path Path where to export files.
#' @param namePrefix Prefix for the files that are going to be saved.
#' @param minCellCount Minimum number of counts to be exported.
#' @param resultId Unique identifier for your set of results.
#'
#' @return Exported object in tbl format.
#'
#' @export
#'
export <- function(x,
                   path,
                   namePrefix = "",
                   minCellCount = 5,
                   resultId = NULL) {
  UseMethod("export")
}

#' Export a generated_cohort_set
#'
#' @param x A generated_cohort_set object.
#' @param path Path where to export files.
#' @param namePrefix Prefix for the files that are going to be saved.
#' @param minCellCount Minimum number of counts to be exported.
#' @param resultId Unique identifier for your set of results.
#'
#' @return Exported generated_cohort_set
#'
#' @export
#'
export.generated_cohort_set <- function(x,
                                        path,
                                        namePrefix = "",
                                        minCellCount = 5,
                                        resultId = NULL) {
  cohortSet(x) |>
    dplyr::inner_join(cohortAttrition(x), by = "cohort_definition_id") |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id) |>
    dplyr::mutate(
      cohort_table_name = attr(x, "tbl_name"),
      cdm_name = cdmName(attr(x, "cdm_reference")),
      result_type = "Cohort details"
    ) |>
    dplyr::relocate(c(
      "result_type", "cdm_name", "cohort_table_name", "cohort_name",
      "cohort_definition_id"
    )) |>
    dplyr::mutate(dplyr::across(
      c("number_subjects", "number_records", "excluded_subjects",
        "excluded_records"),
      ~ dplyr::if_else(
        .x < .env$minCellCount, paste0("<", .env$minCellCount), as.character(.x)
      )
    )) |>
    addResultId(resultId) |>
    saveFile(path, namePrefix, resultId, "cohort_details")
}

#' Export a cdm_reference
#'
#' @param x A cdm_reference object.
#' @param path Path where to export files.
#' @param namePrefix Prefix for the files that are going to be saved.
#' @param minCellCount Minimum number of counts to be exported.
#' @param resultId Unique identifier for your set of results.
#'
#' @return Exported cdm_reference.
#'
#' @export
#'
export.cdm_reference <- function(x,
                                 path,
                                 namePrefix = "",
                                 minCellCount = 5,
                                 resultId = NULL) {
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

  if (person_count < minCellCount) {
    person_count <- paste0("<", minCellCount)
  }

  if (observation_period_info$count < minCellCount) {
    observation_period_info <- dplyr::tibble(
      count = paste0("<", minCellCount), max = NA, min = NA
    )
  }

  cdm_source <- x[["cdm_source"]] |> dplyr::collect()
  if (is.null(cdm_source) || nrow(cdm_source) == 0) {
    cdm_source <- dplyr::tibble(
      vocabulary_version = vocab_version,
      cdm_source_name = "",
      cdm_holder = "",
      cdm_release_date = "",
      cdm_version = attr(x, "cdm_version"),
      source_description = "",
      source_documentation_reference = ""
    )
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
    dplyr::mutate_all(as.character) |>
    addResultId(resultId) |>
    saveFile(path, namePrefix, resultId, "cdm_snapshot")
}

#' Export a summarised_result object
#'
#' @param x A summarised_result object.
#' @param path Path where to export files.
#' @param namePrefix Prefix for the files that are going to be saved.
#' @param minCellCount Minimum number of counts to be exported.
#' @param resultId Unique identifier for your set of results.
#'
#' @return Exported summarised_result.
#'
#' @export
#'
export.summarised_result <- function(x,
                                     path,
                                     namePrefix = "",
                                     minCellCount = 5,
                                     resultId = NULL) {
  name <- attr(x, "summarised_result_name")
  suppress(x, minCellCount = minCellCount)
    addResultId(resultId) |>
    saveFile(path, namePrefix, resultId, name)
}

addResultId <- function(x, resultId) {
  if (!is.null(resultId)) {
    x <- x |>
      dplyr::mutate("result_id" = .env$resultId) |>
      dplyr::relocate("result_id")
  }
  return(x)
}
saveFile <- function(x, path, namePrefix, resultId, nam) {
  readr::write_csv(
    x = x, file = file.path(path, fileName(x, path, namePrefix, resultId, nam))
  )
}
fileName <- function(x, path, namePrefix, resultId, nam) {
  n <- nchar(namePrefix)
  if (n > 0 & substr(namePrefix, n, n) != "_") {
    namePrefix <- paste0(namePrefix, "_")
  }
  if (!is.null(resultId)) {
    resultId <- paste0("_", resultId)
  }
  paste0(
    namePrefix, nam, "_", paste0(unique(x$cdm_name), collapse = "_"), resultId,
    ".csv"
  )
}
getVocabularyVersion <- function(x) {
  vocabVersion <- NULL
  if ("vocabulary_version" %in% colnames(x[["vocabulary"]])) {
    vocabVersion <- x[["vocabulary"]] |>
      dplyr::filter(.data$vocabulary_id == "None") |>
      dplyr::pull(.data$vocabulary_version)
  }
  if (length(vocabVersion) == 0) {
    vocabVersion <- NA_character_
  }
  return(vocabVersion)
}
