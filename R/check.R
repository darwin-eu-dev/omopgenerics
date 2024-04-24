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

#' Check whether a character vector is snake case.
#'
#' @param string A character vector to check if it is snake_case.
#' @param type Can either be "error" or "warning".
#' @param call The corresponding function call is retrieved and mentioned in
#' error messages as the source of the error.
#'
#' @export
#'
#' @return Throws an error/warning if the string is not snake_case
#'
checkSnakeCase <- function(string,
                           type = "error",
                           call = parent.frame()) {
  #input check
  assertCharacter(x = string, call = call)
  assertChoice(x = type, choices = c("error", "warning"), length = 1, call = call)

  # check
  if (any(string != toSnakeCase(string))) {
    name <- substitute(string)
    c("!" = paste0(name, " is not a snake_case string.")) |>
      report(type = type, call = call)
  }

  # return
  return(invisible(NULL))
}

#' Check a cdm object. You can also ensure that it contains a certain set of
#' cdm tables
#'
#' @param cdm A cdm_reference object.
#' @param cdmTables A set of tables that should be in the cdm object.
#' @param call The corresponding function call is retrieved and mentioned in
#' error messages as the source of the error.
#'
#' @export
#'
#' @return A cdm_reference object.
#'
checkCdm <- function(cdm,
                     cdmTables = NULL,
                     call = parent.frame()) {
  assertClass(cdm, "cdm_reference", class = "cdm_reference", call = call)
  assertCharacter(cdmTables)
  if (!is.null(cdmTables)) {
    notPresent <- cdmTables[!cdmTables %in% names(cdm)]
    if (length(notPresent) > 0) {
      cli::cli_abort("{notPresent} not present in the cdm_reference object.")
    }
  }
  return(inviisble(NULL))
}

# INTERNAL
report <- function(x, type, call) {
  if (type == "error") {
    cli::cli_abort(message = x, call = call)
  } else if (type == "warning") {
    cli::cli_warn(message = x, call = call)
  } else if (type == "message") {
    cli::cli_inform(message = x, call = call)
  }
  return(invisible(NULL))
}
toSnakeCase <- function(x) {
  snakecase::to_snake_case(x, numerals = "asis")
}
