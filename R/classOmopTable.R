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

#' Create an cdm table.
#'
#' @param table A table that is part of a cdm.
#' @param src The source of the table.
#' @param name The name of the table.
#'
#' @return A cdm_table object
#'
#' @export
#'
cdmTable <- function(table, src, name) {
  assertClass(src, class = "cdm_source")
  assertCharacter(name, length = 1)
  table <- structure(.Data = table, tbl_source = src, tbl_name = name) |>
    addClass("cdm_table")
  if (any(colnames(table) != tolower(colnames(table)))) {
    cli::cli_abort("A cdm_table must have lowercase column names.")
  }
  return(table)
}

getTableName <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "tbl_name")
}

getTableSource <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "tbl_source")
}

#' Create an omop table from a cdm table.
#'
#' @param table A cdm_table.
#'
#' @return An omop_table object
#'
#' @export
#'
omopTable <- function(table) {
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

#' Create an achilles table from a cdm_table.
#'
#' @param table A cdm_table.
#'
#' @return An achilles_table object
#'
#' @export
#'
achillesTable <- function(table) {
  # create the structure
  assertClass(table, class = "cdm_table")
  table <- addClass(table, "achilles_table")
  name <- getTableName(table)

  # validation
  if (!name %in% achillesTables()) {
    cli::cli_abort("{name} is not one of the achilles omop cdm tables.")
  }

  cols <- omopColumns(table = name)
  checkColumnsCdm(table, name, cols)

  return(table)
}


#' @export
#' @importFrom dplyr collect
collect.cdm_table <- function(x, ...) {
  removeClass(x, "cdm_table") |> dplyr::collect()
}

noReference <- function(x) {
  attr(x, "cdm_reference") <- NULL
  return(x)
}
