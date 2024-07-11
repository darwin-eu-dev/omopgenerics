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

#' Create an achilles table from a cdm_table.
#'
#' @param table A cdm_table.
#' @param version version of the cdm.
#'
#' @return An achilles_table object
#'
#' @export
#'
newAchillesTable <- function(table, version = "5.3") {
  # create the structure
  assertClass(table, class = "cdm_table")
  table <- addClass(table, "achilles_table")
  name <- tableName(table)

  # validation
  if (!name %in% achillesTables()) {
    cli::cli_abort("{name} is not one of the achilles omop cdm tables.")
  }

  cols <- achillesColumns(table = name, version = version)
  checkColumnsCdm(table, name, col)
  table <- castAchillesColumns(table, name, version)

  return(table)
}

#' Create an empty achilles table
#'
#' @param cdm A cdm_reference to create the table.
#' @param name Name of the table to create.
#'
#' @export
#'
#' @return The cdm_reference with an achilles empty table
#'
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' cdm <- emptyCdmReference("my_example_cdm")
#' emptyAchillesTable(cdm = cdm, name = "achilles_results" )
#' }
emptyAchillesTable <- function(cdm, name) {
  assertChoice(name, achillesTables(), length = 1)
  assertClass(cdm, "cdm_reference")
  table <- fieldsTables |>
    dplyr::filter(
      .data$cdm_table_name == .env$name &
        .data$type == "achilles" &
        grepl(cdmVersion(cdm), .data$cdm_version)
    ) |>
    emptyTable()
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = FALSE)
  cdm[[name]] <- cdm[[name]] |> newAchillesTable()
  return(cdm)
}

castAchillesColumns <- function(table, name, version) {
  cols <- fieldsTables |>
    dplyr::filter(
      grepl(.env$version, .data$cdm_version) &
        .data$type == "achilles" & .data$cdm_table_name == .env$name) |>
    dplyr::select("cdm_field_name", "cdm_datatype") |>
    dplyr::mutate("cdm_datatype" = dplyr::case_when(
      grepl("varchar", .data$cdm_datatype) ~ "character",
      .data$cdm_datatype == "float" ~ "numeric",
      .data$cdm_datatype == "datetime" ~ "date",
      .default = .data$cdm_datatype
    ))
  cols <- cols |>
    split(f = as.factor(cols$cdm_field_name)) |>
    lapply(dplyr::pull, "cdm_datatype")
  table <- castColumns(table, cols, name)
  return(table)
}
