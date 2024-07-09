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

#' Create an omop table from a cdm table.
#'
#' @param table A cdm_table.
#'
#' @return An omop_table object
#'
#' @export
#'
newOmopTable <- function(table) {
  # create the structure
  assertClass(table, class = "cdm_table")
  table <- addClass(table, "omop_table")
  name <- tableName(table)

  # validation
  if (!tableName(table) %in% omopTables()) {
    cli::cli_abort("{name} is not one of the omop cdm standard tables.")
  }

  cols <- omopColumns(table = tableName(table))
  checkColumnsCdm(table, name, cols)
  table <- castOmopColumns(table, name)

  return(table)
}

#' Create an empty omop table
#'
#' @param cdm A cdm_reference to create the table.
#' @param name Name of the table to create.
#'
#' @export
#'
#' @return The cdm_reference with an empty cohort table
#'
#' @examples
#' library(omopgenerics)
#'
#' person <- dplyr::tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- dplyr::tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2025-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#'
#' cdm <- emptyOmopTable(cdm, "drug_exposure")
#'
#' cdm$drug_exposure
#'
emptyOmopTable <- function(cdm, name) {
  assertChoice(name, omopTables(), length = 1)
  assertClass(cdm, "cdm_reference")
  table <- emptyOmopTableInternal(name = name, version = cdmVersion(cdm))
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = FALSE)
  cdm[[name]] <- newOmopTable(cdm[[name]])
  return(cdm)
}

castOmopColumns <- function(table, name) {
  cols <- fieldsTables |>
    dplyr::filter(
      .data$type == "cdm_table" & .data$cdm_table_name == .env$name) |>
    dplyr::select("cdm_field_name", "cdm_datatype") |>
    dplyr::mutate("cdm_datatype" = dplyr::if_else(
      grepl("varchar", .data$cdm_datatype), "character", .data$cdm_datatype
    )) |>
    dplyr::group_split(.data$cdm_field_name) |>
    purrr::map(dplyr::pull, "cdm_datatype") |>
    as.list()
}
castColumns <- function(table, cols) {

}
funToCast <- function(x) {
  x[x == "integer"] <- "as.integer"
  x[x == "datetime"] <- "as.Date"
  x[x == "character"] <- "as,character"
  x[x == "date"] <- "as.Date"
  x[x == "float"] <- "as.numeric"
  x[x == "logical"] <- "as.logicla"
  return(x)
}
