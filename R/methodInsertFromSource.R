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

#' Convert a table that is not a cdm_table but have the same original source to
#' a cdm_table.
#' This Table is not meant to be used to insert tables in the cdm, please use
#' insertTable instead.
#'
#' @param cdm A cdm_reference object.
#' @param value A table that shares source with the cdm_reference object.
#'
#' @return A table in the cdm_reference environment
#'
#' @export
#'
insertFromSource <- function(cdm, value) {
  UseMethod("insertFromSource", cdmSource(cdm))
}

#' @export
insertFromSource.local_cdm <- function(cdm, value) {
  assertClass(value, "data.frame")
  src <- cdmSource(cdm)
  value <- newCdmTable(table = value, src = src, name = NA_character_)
  return(value)
}
