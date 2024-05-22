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
#' @param minCellCount Minimum count for suppression purposes.
#' @param fileName Name of the file that will be created. Use \{cdm_name\} to
#' refer to the cdmName of the objects and \{date\} to add the export date.
#' @param path Path where to create the csv file.
#'
#' @export
#'
exportSummarisedResult <- function(...,
                                   minCellCount = 5,
                                   fileName = "results_{cdm_name}_{date}.csv",
                                   path = getwd()) {
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

  # bind and suppress
  results <- bind(...) |> suppress(minCellCount = minCellCount)

  # cdm name
  cdmName <- results$cdm_name |> unique() |> paste0(collapse = "_")
  fileName <- gsub(pattern = "\\{cdm_name\\}", replacement = cdmName, x = fileName)

  # date
  date <- gsub("-", "_", Sys.Date() |> as.character())
  fileName <- gsub(pattern = "\\{date\\}", replacement = date, x = fileName)

  # to tibble + pivot settings
  x <- results |>
    dplyr::as_tibble() |>
    dplyr::union_all(results |> pivotSettings() |> dplyr::as_tibble())

  utils::write.csv(
    x, file = file.path(path, fileName), row.names = FALSE
  )
}

pivotSettings <- function(x) {
  x |>
    settings() |>
    dplyr::mutate(dplyr::across(!"result_id", ~ as.character(.x))) |>
    tidyr::pivot_longer(
      cols = !"result_id",
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::filter(!is.na(.data$estimate_value)) |>
    dplyr::inner_join(
      x |>
        settings() |>
        variableTypes() |>
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
    dplyr::arrange(.data$result_id) |>
    dplyr::left_join(
      x |>
        dplyr::select("result_id", "cdm_name") |>
        dplyr::distinct() |>
        dplyr::group_by(.data$result_id) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup(),
      by = "result_id"
    )
}
variableTypes <- function(table) {
  assertTable(table, class = "data.frame")
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
