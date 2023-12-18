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

#' `cdm_reference` objects constructor
#'
#' @param cdmTables List of standard tables in the OMOP Common Data Model.
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmReference <- function(cdmTables) {

  class(cdmTables) <- "cdm_reference"

  return(cdmTables)
}

#' Subset a cdm reference object.
#'
#' @param x A cdm reference
#' @param name The name or index of the table to extract from the cdm object.
#'
#' @return A single cdm table reference
#' @export
`[[.cdm_reference` <- function(x, name) {
  print("out")
  x_raw <- unclass(x)
  tbl <- x_raw[[name]]
  attr(tbl, "reference") <- x
  return(tbl)
}
