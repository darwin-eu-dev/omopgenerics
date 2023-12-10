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

#' Insert a table to a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The table in the cdm reference or the cdm reference.
#'
insertTable <- function(src, name, table) {
  assertCharacter(name, length = 1, minNumCharacter = 1, na = TRUE)
  assertClass(table, "data.frame")
  UseMethod("insertTable")
}

#' Insert a table to a cdm object.
#'
#' @param src A cdm reference or the source of a cdm reference.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The cdm reference.
#'
insertTable.cdm_reference <- function(src, name, table) {
  table <- insertTable(src = getCdmSource(src), name = name, table = table)
  attr(table, "cdm_reference") <- src
  return(table)
}

#' Insert a table to a local cdm object.
#'
#' @param src The source of a local cdm.
#' @param name Name of the table to insert.
#' @param table Table to insert to the cdm.
#'
#' @export
#'
#' @return The table in the cdm reference.
#'
insertTable.local_cdm <- function(src, name, table) {
  attr(table, "tbl_name") <- name
  return(table)
}
