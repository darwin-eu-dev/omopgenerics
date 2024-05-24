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

#' Validate a cdm reference object.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A cdm_reference object.
#'
#' @export
#'
validateName <- function(name, cdm = NULL) {
  nm <- paste0(substitute(name), collapse = "")
  if (!is.character(name) && length(name) == 1) {
    cli::cli_abort("`{nm}` must be a character of length 1.")
  }
  newName <- toSnakeCase(name)
  if (newName != name) {
    cli::cli_inform(c("!" = "`{nm}` was modified: {name} -> {newName}"))
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      cli::cli_inform(c("!" = "There already exist a table in cdm[[{newName}]]. It will be overwritten."))
    }
  }
  return(newName)
}
