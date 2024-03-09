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

#' Read a table from the cdm_source and add it to to the cdm.
#'
#' @param cdm A cdm reference.
#' @param name Name of a table to read in the cdm_source space.
#'
#' @export
#'
#' @return A cdm_reference with new table.
#'
readSourceTable <- function(cdm, name) {
  UseMethod("readSourceTable")
}

#' @export
readSourceTable.cdm_reference <- function(cdm, name) {
  tablesToRead <- listSourceTables(cdm)
  assertCharacter(name, length = 1)
  if (!name %in% tablesToRead) {
    cli::cli_abort(
      "{name} is not a table that could be read from the cdm_source. Please use
      listSourceTables(cdm) to see the available tables."
    )
  }
  cdm[[name]] <- readSourceTable(cdmSource(cdm), name)
  return(cdm)
}
