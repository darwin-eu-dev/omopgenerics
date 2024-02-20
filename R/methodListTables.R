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

#' List tables that can be accessed though a cdm object.
#'
#' @param cdm A cdm reference or the source of a cdm reference.
#'
#' @export
#'
#' @return A character vector with the names of tables.
#'
listTables <- function(cdm) {
  UseMethod("listTables")
}

#' @export
listTables.cdm_reference <- function(cdm) {
  listTables(cdmSource(cdm))
}

#' @export
listTables.local_cdm <- function(cdm) {
  character()
}
