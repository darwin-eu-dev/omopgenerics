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
    dplyr::mutate("cdm_datatype" = dplyr::case_when(
      grepl("varchar", .data$cdm_datatype) ~ "character",
      .data$cdm_datatype == "float" ~ "numeric",
      .data$cdm_datatype == "datetime" ~ "date",
      .default = .data$cdm_datatype
    ))
  cols <- cols |>
    split(f = as.factor(cols$cdm_field_name)) |>
    lapply(dplyr::pull, "cdm_datatype")
  castColumns(table, cols, name)
}
castColumns <- function(table, cols, name) {
  colsToCast <- detectColsToCast(table, cols)
  if (length(colsToCast) > 0) {
    warnColsToCast(colsToCast, name)
    table <- castTableColumns(table, colsToCast)
  }
  return(table)
}
detectColsToCast <- function(table, cols) {
  colTypes <- table |>
    utils::head(1) |>
    dplyr::collect() |>
    purrr::map(dplyr::type_sum) |>
    lapply(assertClassification)
  cols <- cols[sort(names(colTypes))]
  colTypes <- colTypes[sort(names(cols))]
  cols <- setdiff(cols, colTypes)
  colsToCast <- list("new" = cols, "old" = colTypes[names(cols)])
  return(colsToCast)
}
warnColsToCast <- function(colsToCast, name) {
  msg <- NULL
  nms <- names(colsToCast$new)
  for (nm in nms) {
    msg <- c(msg, "*" = paste0(nm, " from ", colsToCast$old[[nm]], " to ", colsToCast$new[[nm]]))
  }
  msg <- c("!" = "{length(colsToCast$new)} casted in {.strong {name}} as do not match expected column type:", msg) |>
    glue::glue()
  cli::cli_warn(message = msg)
}
castTableColumns <- function(table, colsToCast) {
  cols <- colsToCast$new |> funToCast()
  qC <- paste0(cols, "(.data[['", names(cols), "']])") |>
    rlang::parse_exprs() |>
    rlang::set_names(names(cols))
  table <- table |> dplyr::mutate(!!!qC)
  return(table)
}
funToCast <- function(x) {
  x[x == "integer"] <- "as.integer"
  x[x == "character"] <- "as.character"
  x[x == "date"] <- "as.Date"
  x[x == "numeric"] <- "as.numeric"
  x[x == "logical"] <- "as.logicla"
  return(x)
}
