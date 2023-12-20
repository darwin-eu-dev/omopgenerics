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

#' Insert a table to a cdm object.
#'
#' @param cdm A cdm reference or the source of a cdm reference.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#' @param overwrite Whether to overwrite an existent table.
#'
#' @export
#'
#' @return The cdm reference.
#'
insertTable <- function(cdm, name, table, overwrite = TRUE) {
  assertCharacter(name, length = 1, minNumCharacter = 1, na = TRUE)
  UseMethod("insertTable")
}

#' @export
insertTable.cdm_reference <- function(cdm, name, table, overwrite) {
  value <- insertTable(
    cdm = getCdmSource(cdm), name = name, table = table, overwrite = overwrite
  )
  attr(value, "cdm_reference") <- cdm
  cdm[[name]] <- value
  return(cdm)
}

#' @export
insertTable.local_cdm <- function(cdm, name, table, ...) {
  attr(table, "tbl_name") <- name
  return(table)
}
