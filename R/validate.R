# Copyright 2024 DARWIN EU (C)
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

#' Validate that the a variable is a valid name for a table in the cdm.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A cdm_reference object. It will trow a warning if a table named
#' name already exists in the cdm.
#' @param validation How to perform validation: "strict", "relaxed", "none".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateName <- function(name,
                         cdm = NULL,
                         validation = "strict",
                         call = parent.frame()) {
  assertValidation(validation)
  nm <- substitute(name) |> utils::capture.output()
  assertCharacter(name, length = 1, call = call)
  newName <- toSnakeCase(name)
  if (newName != name) {
    if (validation == "strict") {
      cli::cli_abort(c("!" = "`{nm}` is not snake_case it should be modified to: {newName}"))
    } else if (validation == "relaxed") {
      cli::cli_warn(c("!" = "`{nm}` was modified: {name} -> {newName}"))
    }
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      if (validation == "strict") {
        cli::cli_abort(c("!" = "There already exist a table named {.var {newName}}."))
      } else if (validation == "relaxed") {
        cli::cli_warn(c("!" = "There already exist a table named {.var {newName}}. It will be overwritten."))
      }
    }
  }
  return(newName)
}

#' Validate a cohort table input.
#'
#' @param cohort Object to be validated as a valid cohort input.
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on
#' or after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have
#' overlapping cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#' @param validation How to perform validation: "strict", "relaxed", "none".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateCohort <- function(cohort,
                           checkEndAfterStart = TRUE,
                           checkOverlappingEntries = TRUE,
                           checkMissingValues = TRUE,
                           checkInObservation = TRUE,
                           validation = "strict",
                           call = parent.frame()) {
  assertValidation(validation)
  assertLogical(checkEndAfterStart, length = 1)
  assertLogical(checkOverlappingEntries, length = 1)
  assertLogical(checkMissingValues, length = 1)
  assertLogical(checkInObservation, length = 1)

  assertClass(cohort, class = c("cohort_table", "cdm_table"), all = TRUE, call = call)
  # columns
  notPresent <- cohortColumns("cohort")[!cohortColumns("cohort") %in% colnames(cohort)]
  if (length(notPresent) > 0) {
    if (validation == "strict") {
      cli::cli_abort(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    } else if (validation == "relaxed") {
      cli::cli_warn(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    }
  }
  cohort <- cohort |> dplyr::relocate(dplyr::any_of(cohortColumns("cohort")))

  if(isTRUE(checkEndAfterStart)){
    cohort <- checkStartEnd(cohort = cohort, validation = validation, call = call)
  }
  if(isTRUE(checkOverlappingEntries)){
    cohort <- checkOverlap(cohort = cohort, validation = validation, call = call)
  }
  if(isTRUE(checkMissingValues)){
    cohort <- checkNaCohort(cohort = cohort, validation = validation, call = call)
  }
  if(isTRUE(checkInObservation)){
    cohort <- checkObservationPeriod(cohort = cohort, validation = validation, call = call)
  }
  return(cohort)
}

#' Validate A cohortId input.
#'
#' @param cohortId A cohortId vector to be validated.
#' @param cohort A cohort_table object.
#' @param validation How to perform validation: "strict", "relaxed", "none".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateCohortId <- function(cohortId,
                             cohort,
                             validation = "strict",
                             call = parent.frame()) {
  assertValidation(validation)
  assertNumeric(cohortId, integerish = TRUE, null = TRUE, min = 1, unique = TRUE, call = call)
  assertClass(cohort, class = "cohort_table", call = call)
  possibleCohortIds <- settings(cohort) |>
    dplyr::pull("cohort_definition_id") |>
    as.integer()
  if (is.null(cohortId)) {
    cohortId <- possibleCohortIds
  } else {
    cohortId <- as.integer(cohortId)
    notPresent <- cohortId[!cohortId %in% possibleCohortIds]
    cohortId <- cohortId[cohortId %in% possibleCohortIds]
    if (length(notPresent) > 0) {
      if (validation == "strict" | length(cohortId) == 0) {
        cli::cli_abort("cohort definition id: {notPresent} not defined in settings.", call = call)
      } else if (validation == "relaxed") {
        cli::cli_warn(c("!" = "cohort definition id: {notPresent} not considered as they are not defined in settings."), call = call)
      }
    }
  }
  return(cohortId)
}

assertValidation <- function(validation, call = parent.frame()) {
  assertChoice(validation, choices = c("strict", "relaxed", "none"),
               length = 1, call = call)
}
