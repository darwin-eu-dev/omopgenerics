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
#'
#' @return An achilles_table object
#'
#' @export
#'
newAchillesTable <- function(table) {
  # create the structure
  assertClass(table, class = "cdm_table")
  table <- addClass(table, "achilles_table")
  name <- getTableName(table)

  # validation
  if (!name %in% achillesTables()) {
    cli::cli_abort("{name} is not one of the achilles omop cdm tables.")
  }

  cols <- achillesColumns(table = name)
  checkColumnsCdm(table, name, cols)

  return(table)
}

#' Create an empty achilles table
#'
#' @param name Name of the table to create.
#' @param cdm A cdm_reference to create the table.
#'
#' @noRd
#'
#' @return The cdm_reference with an achilles empty table
#'
emptyAchillesTable <- function(name, cdm) {
  # check input
  assertChoice(name, achillesTables(), length = 1)
  assertClass(cdm, "cdm_reference")

  # create tibble
  # TODO correct column type
  x <- achillesColumns(name) |>
    rlang::rep_named(list(character())) |>
    dplyr::as_tibble()
  cdm <- insertTable(cdm = cdm, name = name, table = x, overwrite = FALSE)

  # validate
  cdm[[name]] <- cdm[[name]] |> newAchillesTable()

  return(cdm)
}
