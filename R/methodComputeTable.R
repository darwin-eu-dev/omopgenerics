# Copyright 2023 DARWIN EU (C)
#
# This file is part of OMOPGenerics
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

#' Compute a cdm table.
#'
#' @param x A table.
#' @param cdm A cdm reference.
#' @param name Name of the table to insert.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
computeTable <- function(x, cdm, name = NA_character_) {
  assertCharacter(name, length = 1, minNumCharacter = 1, na = TRUE)
  assertClass(cdm, "cdm_reference")
  UseMethod("computeTable", getCdmSource(cdm))
}

#' @export
computeTable.local_cdm <- function(x, cdm, name) {
  attr(x, "tbl_name") <- name
  attr(x, "cdm_reference") <- cdm
  return(x)
}
