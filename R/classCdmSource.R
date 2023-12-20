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

#' Create a cdm source object.
#'
#' @param src Source to a cdm object.
#' @param sourceName Name of the source.
#' @param sourceType Type of the source object.
#'
#' @export
#'
#' @return A validated cdm source object.
#'
cdmSource <- function(src, sourceName, sourceType) {
  # initial check
  assertCharacter(sourceName, length = 1, minNumCharacter = 1)
  assertCharacter(sourceType, length = 1, minNumCharacter = 1)

  # assign class
  src <- newCdmSource(src = src, sourceName = sourceName, sourceType = sourceType)

  # validate source
  src <- validateCdmSource(src = src)

  return(src)
}

newCdmSource <- function(src, sourceName, sourceType) {
  src <- addClass(src, "cdm_source")
  attr(src, "source_name") <- sourceName
  attr(src, "source_type") <- sourceType
  return(src)
}
validateCdmSource <- function(src) {
  # toy data
  name <- paste0(c(sample(letters, 5, replace = TRUE), "_test_table"), collapse = "")
  table <- datasets::cars

  # create mock cdm
  cdm <- cdmReference(
    cdmTables = list(
      person = dplyr::tibble(
        person_id = 1, gender_concept_id = 0, year_of_birth = 2000,
        race_concept_id = 0, ethnicity_concept_id = 0
      ),
      observation_period = dplyr::tibble(
        observation_period_id = 1, person_id = 1,
        observation_period_start_date = as.Date("2010-01-01"),
        observation_period_end_date = as.Date("2029-12-31"),
        period_type_concept_id = 0
      )
    ),
    cdmName = "mock",
    cdmSource = src
  )

  # insert table
  cdm <- insertTable(cdm = cdm, name = name, table = table)
  validateX(x = cdm[[name]], name = name, fun = "insertTable")

  # check inserted table
  attr(table, "tbl_name") <- name
  attr(table, "cdm_reference") <- cdm
  if (!identical(unclass(dplyr::collect(cdm[[name]])), unclass(table))) {
    cli::cli_abort("The inserted table was not the same than the original one.")
  }

  # compute inserted table
  cdm[[name]] <- cdm[[name]] |> compute(name = name, temporary = FALSE)
  validateX(x = cdm[[name]], name = name, fun = "compute")

  # drop table
  cdm <- dropTable(cdm = cdm, name = dplyr::all_of(name))

  return(invisible(src))
}

validateX <- function(x, name, fun) {
  if (!identical(attr(x, "tbl_name"), name)) {
    cli::cli_abort("table name is not correctly assigned in {fun}")
  }
  if (!"cdm_table" %in% class(x)) {
    cli::cli_abort("cdm_table class is not correctly assigned in {fun}")
  }
  return(invisible(TRUE))
}

#' @export
print.cdm_source <- function(x, ...) {
  cli::cli_inform(
    "This is a {attr(x, 'source_type')} cdm source of {attr(x, 'source_name')}"
  )
}
