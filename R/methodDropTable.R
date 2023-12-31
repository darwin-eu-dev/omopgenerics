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
dropTable <- function(cdm, name) {
  UseMethod("dropTable")
}

#' @export
dropTable.cdm_reference <- function(cdm, name) {
  dropTable(getCdmSource(cdm), name = name)
  allTables <- names(cdm)
  names(allTables) <- names(cdm)
  toDrop <- names(tidyselect::eval_select(dplyr::any_of(name), data = allTables))
  if (length(toDrop) > 0) {
    for (nm in toDrop) {
      cdm[[nm]] <- NULL
    }
  }
  return(invisible(cdm))
}

#' @export
dropTable.local_cdm <- function(cdm, name) {
  return(invisible(TRUE))
}
