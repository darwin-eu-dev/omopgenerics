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

#' Export a summarised_result object to a csv file.
#'
#' @param ... A set of summarised_result objects.
#' @param minCelCount Minimum count for suppression purposes.
#' @param fileName Name of the file that will be created. Use {cdm_name} to
#' refer to the cdmName of the objects and {date} to add the export date.
#' @param path Path where to create the csv file.
#'
#' @export
#'
exportSummarisedResult <- function(...,
                                   minCellCount = 5,
                                   fileName = "results_{cdm_name}_{date}.csv",
                                   path = here::here()) {
  # initial checks
  results <- list(...)
  assertList(x = results, class = "summarised_result")
  assertCharacter(fileName, length = 1, minNumCharacter = 1)
  assertCharacter(path, length = 1, minNumCharacter = 1)
  if (!dir.exists(path)) {
    cli::cli_abort("path: {path}, does not exist")
  }
  if (tools::file_ext(fileName) != "csv") {
    fileName <- paste0(fileName, ".csv")
  }

  results <- bind(...) |> suppress(minCellCount = minCellCount)

  cdmName <- results$cdm_name |> unique()
  date <- Sys.Date()

  set <- settings(results)

}

appendSettings <- function(x, colsSettings) {
  # initial checks
  assertCharacter(colsSettings, null = TRUE)
  assertTibble(x, columns = colsSettings)

  # check if there is already a x id
  if("result_id" %in% colnames(x)) {
    ids <- x |>
      dplyr::select(dplyr::all_of(c("result_id", colsSettings))) |>
      dplyr::distinct()
    x <- x |>
      dplyr::select(!dplyr::all_of(colsSettings))
    # check result_id linked to one set of settings:
    if (nrow(ids) != length(unique(ids$result_id))) {
      cli::cli_abort("Settings do not match result ids.")
    }
  } else {
    ids <- x |>
      dplyr::select(dplyr::all_of(colsSettings)) |>
      dplyr::distinct() |>
      dplyr::mutate("result_id" = as.integer(dplyr::row_number()))
    x <- x |>
      dplyr::left_join(ids, by = colsSettings) |>
      dplyr::select(!dplyr::all_of(colsSettings)) |>
      dplyr::relocate("result_id")
  }
  # columns to match settings - result
  colsToMatch <- c("result_id", "cdm_name", "result_type", "package_name", "package_version")
  colsToMatch <- colsToMatch[colsToMatch %in% colnames(x)]
  # format settings to summarised
  settingsIds <- ids |>
    dplyr::mutate(dplyr::across(dplyr::all_of(colsSettings), ~ as.character(.x))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(colsSettings),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::inner_join(
      variableTypes(ids) |>
        dplyr::select(
          "estimate_name" = "variable_name", "estimate_type" = "variable_type"
        ) |>
        dplyr::mutate("estimate_type" = dplyr::if_else(
          .data$estimate_type == "categorical", "character", .data$estimate_type
        )),
      by = "estimate_name"
    ) |>
    dplyr::mutate(
      "estimate_value" = as.character(.data$estimate_value),
      "variable_name" = "settings",
      "variable_level" = NA_character_,
      "group_name" = "overall",
      "group_level" = "overall",
      "strata_name" = "overall",
      "strata_level" = "overall",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    dplyr::left_join(
      x |> dplyr::distinct(dplyr::across(dplyr::all_of(colsToMatch))),
      by = "result_id"
    )
  # check if there are non-summarised result columns
  nonS <- !colnames(x) %in% c(omopgenerics::resultColumns(), colsSettings)
  if(sum(nonS) > 0) {
    toFillNa <- colnames(x)[nonS]
    settingsIds[toFillNa] = NA_character_
  }
  settingsIds <- settingsIds |>
    dplyr::select(dplyr::all_of(colnames(x)))
  # append settings
  x <- settingsIds |>
    dplyr::union_all(x) |>
    dplyr::arrange(.data$result_id)
  return(x)
}


variableTypes <- function(table) {
  assertTibble(table)
  if (ncol(table) > 0) {
    x <- dplyr::tibble(
      "variable_name" = colnames(table),
      "variable_type" = lapply(colnames(table), function(x) {
        table |>
          dplyr::select(dplyr::all_of(x)) |>
          utils::head(1) |>
          dplyr::pull() |>
          dplyr::type_sum() |>
          assertClassification()
      }) |> unlist()
    )
  } else {
    x <- dplyr::tibble(
      "variable_name" = character(),
      "variable_type" = character()
    )
  }
  return(x)
}

assertClassification <- function(x) {
  switch (
    x,
    "chr" = "character",
    "fct" = "character",
    "ord" = "character",
    "date" = "date",
    "dttm" = "date",
    "lgl" = "logical",
    "drtn" = "numeric",
    "dbl" = "numeric",
    "int" = "integer",
    "int64" = "integer",
    NA_character_
  )
}
