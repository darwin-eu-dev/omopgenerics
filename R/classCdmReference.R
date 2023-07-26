# Copyright 2023 DARWIN EU (C)
#
# This file is part of CDMUtilities
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

#' CdmReference objects constructor
#'
#' @param cdmTables List of tables (tbl_sql, tbl or data.frame) that contains a
#' reference to an OMOP Common data model.
#' @param cdmName Name of the cdm.
#'
#' @return A cdm reference
#'
#' @export
#'
newCdmReference <- function(cdmTables, cdmName) {
  # initial input check
  checkInput(cdmTables = cdmTables, cdmName = cdmName)

  attr(cdmTables, "cdm_name") <- cdmName
  class(cdmTables) <- "cdm_reference"
  return(cdmTables)
}

#' Subset a cdm reference object
#'
#' @param x A cdm reference
#' @param name The name of the table to extract from the cdm object
#'
#' @return A single cdm table reference
#' @export
`$.cdm_reference` <- function(x, name) {
  x[[name]]
}

#' Subset a cdm reference object
#'
#' @param x A cdm reference
#' @param i The name or index of the table to extract from the cdm object
#'
#' @return A single cdm table reference
#' @export
`[[.cdm_reference` <- function(x, i) {
  x_raw <- unclass(x)
  tbl <- x_raw[[i]]

  if(is.null(tbl)) return(NULL)

  attr(tbl, "cdm_reference") <- x
  return(tbl)
}

#' Print a CDM reference object
#'
#' @param x A cdm_reference object
#' @param ... Included for compatibility with generic. Not used.
#'
#' @return Invisibly returns the input
#' @export
print.cdm_reference <- function(x, ...) {
  type <- class(x[[1]])[[1]]
  cli::cat_line(glue::glue("# OMOP CDM reference ({type})"))
  cli::cat_line("")
  cli::cat_line(paste("Tables:", paste(names(x), collapse = ", ")))
  invisible(x)
}
