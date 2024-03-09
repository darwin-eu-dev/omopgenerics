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

#' This is an internal developer focused function that creates a cdm_table from
#' a table that shares the source but it is not a cdm_table.
#' Please use insertTable if you want to insert a table to a cdm_reference
#' object.
#'
#' @param src A cdm_source object.
#' @param value A table that shares source with the cdm_reference object.
#'
#' @return A cdm_table.
#'
#' @export
#'
cdmTableFromSource <- function(src, value) {
  UseMethod("cdmTableFromSource", src)
}

#' @export
cdmTableFromSource.local_cdm <- function(src, value) {
  assertClass(value, "data.frame")
  value <- newCdmTable(table = value, src = src, name = NA_character_)
  return(value)
}
