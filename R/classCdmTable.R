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
  assertClass(src, class = "cdm_source",
              msg = "`src` does not have the class: cdm_source")
  assertCharacter(name, length = 1, na = TRUE,
                  msg = "`name` is not a character vector of length 1")
  table <- structure(.Data = table, tbl_source = src, tbl_name = name) |>
    addClass("cdm_table")
  if (any(colnames(table) != tolower(colnames(table)))) {
    cli::cli_abort("A cdm_table must have lowercase column names.")
  }
  return(table)
}


#' Get the `cdm_reference` of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A cdm_reference.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' cdmReference(cdm$person)
#' }
cdmReference <- function(table) {
  assertClass(table, "cdm_table")
  attr(table, "cdm_reference")
}

#' Get the table name of a `cdm_table`.
#'
#' @param table A cdm_table.
#'
#' @return A character with the name.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' tableName(cdm$person)
#' }
tableName <- function(table) {
  assertClass(table, "cdm_table",
              msg = "`table` does not have the class: cdm_table")
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
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2023-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock"
#' )
#'
#' tableSource(cdm$person)
#' }
tableSource <- function(table) {
  assertClass(table, "cdm_table",
              msg = "`table` does not have the class: cdm_table")
  attr(table, "tbl_source")
}

#' @export
#' @importFrom dplyr collect
collect.cdm_table <- function(x, ...) {
  x <- removeClass(x, "cdm_table")
  if (any(colnames(x) != tolower(colnames(x)))) {
    # TO CHANGE TO ERROR IN NEW RELEASE
    cli::cli_warn(c("!" = "A cdm_table must have lowercase column names."))
  }
  x <- x |> dplyr::collect()
  attr(x, "tbl_name") <- NULL
  attr(x, "tbl_source") <- NULL
  attr(x, "cdm_reference") <- NULL
  return(x)
}

noReference <- function(x) {
  attr(x, "cdm_reference") <- NULL
  return(x)
}
