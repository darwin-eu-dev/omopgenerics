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
#' @param cohortTables List of tables that contains `generated_cohort_set`
#' objects.
#' @param cdmName Name of the cdm.
#' @param sourceCdm Source of the cdm object.
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmReference <- function(cdmTables, cohortTables = list(), cdmName, sourceCdm = NULL) {

  cdm <- c(cdmTables, cohortTables)
  attr(cdm, "cdm_name") <- cdmName
  attr(cdm, "cdm_version") <- "5.3"
  attr(cdm, "cdm_source") <- sourceCdm
  class(cdm) <- "cdm_reference"

  return(cdm)
}

#' Subset a cdm reference object.
#'
#' @param x A cdm reference.
#' @param name The name of the table to extract from the cdm object.
#'
#' @return A single cdm table reference
#' @export
`$.cdm_reference` <- function(x, name) {
  x[[name]]
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
  attr(tbl, "cdm_reference") <- x
  return(tbl)
}

#' Assign an table to a cdm reference.
#'
#' @param cdm A cdm reference.
#' @param name Name where to assign the new table.
#' @param value Table with the same source than the cdm object.
#'
#' @return The cdm reference.
#'
#' @export
#'
`$<-.cdm_reference` <- function(cdm, name, value) {
  cdm[[name]] <- value
  return(cdm)
}

#' Assign an table to a cdm reference.
#'
#' @param cdm A cdm reference.
#' @param name Name where to assign the new table.
#' @param value Table with the same source than the cdm object.
#'
#' @return The cdm reference.
#'
#' @export
#'
`[[<-.cdm_reference` <- function(cdm, name, value) {
  print("in")
  attr(value, "cdm_reference") <- NULL
  originalClass <- class(cdm)
  cdm <- unclass(cdm)
  cdm[[name]] <- value
  class(cdm) <- originalClass
  return(cdm)
}

