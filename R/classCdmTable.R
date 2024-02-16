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
newCdmTable <- function(table, src, name) {
  assertClass(src, class = "cdm_source")
  assertCharacter(name, length = 1, na = TRUE)
  table <- structure(.Data = table, tbl_source = src, tbl_name = name) |>
    addClass("cdm_table")
  if (any(colnames(table) != tolower(colnames(table)))) {
    cli::cli_abort("A cdm_table must have lowercase column names.")
  }
  return(table)
}


#' Obtain the `cdm_reference` of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A cdm_reference.
#'
#' @export
#'
cdmReference <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "cdm_reference")
}

#' Read the table name of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A character with the name.
#'
#' @export
#'
tableName <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "tbl_name")
}

#' Get the table source of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A cdm_source object.
#'
#' @export
#'
tableSource <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "tbl_source")
}

#' @export
#' @importFrom dplyr collect
collect.cdm_table <- function(x, ...) {
  x <- removeClass(x, "cdm_table") |> dplyr::collect()
  attr(x, "tbl_name") <- NULL
  attr(x, "tbl_source") <- NULL
  attr(x, "cdm_reference") <- NULL
  return(x)
}

noReference <- function(x) {
  attr(x, "cdm_reference") <- NULL
  return(x)
}
