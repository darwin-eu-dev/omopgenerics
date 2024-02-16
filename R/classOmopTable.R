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
  name <- getTableName(table)

  # validation
  if (!getTableName(table) %in% omopTables()) {
    cli::cli_abort("{name} is not one of the omop cdm standard tables.")
  }

  cols <- omopColumns(table = getTableName(table))
  checkColumnsCdm(table, name, cols)

  return(table)
}

#' Create an empty omop table
#'
#' @param name Name of the table to create.
#' @param cdm A cdm_reference to create the table.
#'
#' @export
#'
#' @return The cdm_reference with an empty cohort table
#'
emptyOmopTable <- function(name, cdm) {
  assertChoice(name, omopTables(), length = 1)
  assertClass(cdm, "cdm_reference")
  table <- fieldsTables |>
    dplyr::filter(
      .data$cdm_table_name == .env$name &
        .data$type == "cdm_table" &
        grepl(cdmVersion(cdm), .data$cdm_version)
    ) |>
    emptyTable()
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = FALSE)
  cdm[[name]] <- newOmopTable(cdm[[name]])
  return(cdm)
}
