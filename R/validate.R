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

#' Input validation of arguments, arguments will be corrected accordingly.
#' You can check the possible validated arguments and their validation in
#' `argumentsValidation()`.
#'
#' @param ... Named arguments to validate.
#' @param call Call object passed to cli messages.
#' @param .options named list of internal arguments to be passed to the
#' validation functions
#'
#' @export
#'
inputValidation <- function(..., call = parent.frame(), .options = list()) {

}

#' Details on the performed validation per argument.
#'
#' @param argumentName The of the argument to see the details. If NULL all
#' available arguments are displayed.
#' @param verbose Whether to print the details
#'
#' @return A tibble with the details of the validation (invisible).
#'
#' @export
#'
#' @examples
#' argumentsValidation("cohortId")
#' argumentsValidation()
#'
argumentsValidation <- function(argumentName = NULL, verbose = TRUE) {
  # input validation
  assertCharacter(argumentName, null = TRUE)

  if (is.null(argumentName)) {
    x <- argumentValidation
  } else {
    x <- argumentValidation |>
      dplyr::filter(.data$argument_name %in% .env$argumentName)
  }

  if (nrow(x) == 0) return(invisible(x))

  xx <- "\u00a0\u00a0\u00a0\u00a0-" |> glue::glue()
  cli::cli_h1("Input validation")
  for (k in seq_len(nrow(x))) {
    cli::cli_h3("Argument: {.field {x$argument_name[[k]]}}")
    cli::cli_inform(c("*" = "{.string Validation}: {x$validation[[k]]}"))
    for (i in seq_along(x$required_arguments[[k]])) {
      if (i == 1) cli::cli_inform(c("*" = "{.string Required arguments}:"))
      nm <- names(x$required_arguments[[k]])
      mes <- x$required_arguments[[k]][[i]]
      cli::cli_text("{xx} {.var {nm}}: {mes}.")
    }
    for (i in seq_along(x$optional_arguments[[k]])) {
      if (i == 1) cli::cli_inform(c("*" = "{.string Optional arguments}:"))
      nm <- names(x$optional_arguments[[k]])
      mes <- x$optional_arguments[[k]][[i]]
      cli::cli_text("{xx} {.var {nm}}: {mes}.")
    }
  }
  cli::cli_text()

  return(invisible(x))
}


#' Validate that the a variable is a valid name for a table in the cdm. A
#' message will be shown if name had to be modified and/or the name is already
#' part of the cdm.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A cdm_reference object. It will tro
#'
#' @export
#'
validateName <- function(name, cdm = NULL) {
  nm <- paste0(substitute(name), collapse = "")
  assertCharacter(name, length = 1)
  newName <- toSnakeCase(name)
  if (newName != name) {
    cli::cli_inform(c("!" = "`{nm}` was modified: {name} -> {newName}"))
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      cli::cli_inform(c("!" = "There already exist a table named {.var {newName}}. It will be overwritten."))
    }
  }
  return(newName)
}

#' Validate a cohort table input.
#'
#' @param cohort Object to be validated as a valid cohort input.
#'
#' @export
#'
validateCohort <- function(cohort) {
  assertClass(cohort, class = c("cohort_table", "cdm_table"), all = TRUE)
}

#' Validate A cohortId input.
#'
#' @param cohortId A cohortId vector to be validated.
#' @param cohort A cohort_table object.
#'
#' @export
#'
validateCohortId <- function(cohortId, cohort) {
  assertNumeric(cohortId, integerish = TRUE, null = TRUE, min = 1, unique = TRUE)
  possibleCohortIds <- settings(cohort) |>
    dplyr::pull("cohort_definition_id") |>
    as.integer()
  if (is.null(cohortId)) {
    cohortId <- possibleCohortIds
  } else {
    cohortId <- as.integer(cohortId)
    notPresent <- cohortId[!cohortId %in% possibleCohortIds]
    if (length(notPresent) > 0) {
      cli::cli_abort("cohort definition id: {notPresent} not defined in settings.")
    }
  }
  return(cohortId)
}
