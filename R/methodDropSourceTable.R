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

#' Drop a table from a cdm object.
#'
#' @param cdm A cdm reference.
#' @param name Name(s) of the table(s) to insert. Tidyselect statements are
#' supported.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
dropSourceTable <- function(cdm, name) {
  UseMethod("dropSourceTable")
}

#' @export
dropSourceTable.cdm_reference <- function(cdm, name) {
  namesCdm <- names(cdm)
  namesSource <- listSourceTables(cdm = cdm)
  toDrop <- c(namesCdm, namesSource) |>
    unique() |>
    selectTables(name = name)
  toDropCdm <- namesCdm[namesCdm %in% toDrop]
  toDropSource <- namesSource[namesSource %in% toDrop]
  dropSourceTable(cdmSource(cdm), name = toDropSource)
  if (length(toDropCdm) > 0) {
    for (nm in toDropCdm) {
      cdm[[nm]] <- NULL
    }
  }
  return(invisible(cdm))
}

selectTables <- function(tables, name) {
  tables |>
    as.list() |>
    rlang::set_names(nm = tables) |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::any_of(name)) |>
    colnames()
}

#' @export
dropSourceTable.local_cdm <- function(cdm, name) {
  return(invisible(TRUE))
}
