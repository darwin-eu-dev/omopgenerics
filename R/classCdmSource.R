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

#' Create a cdm source object.
#'
#' @param src Source to a cdm object.
#' @param sourceType Type of the source object.
#'
#' @export
#'
#' @return A validated cdm source object.
#'
newCdmSource <- function(src, sourceType) {
  # initial check
  assertCharacter(sourceType, length = 1, minNumCharacter = 1)

  # assign class
  src <- constructCdmSource(src = src, sourceType = sourceType)

  # validate source
  src <- validateCdmSource(src = src)

  return(src)
}

constructCdmSource <- function(src, sourceType) {
  structure(
    .Data = src, source_type = sourceType, class = c(class(src), "cdm_source")
  )
}
validateCdmSource <- function(src) {
  # toy data
  name <- paste0(c(sample(letters, 5, replace = TRUE), "_test_table"), collapse = "")
  table <- datasets::cars |>
    dplyr::arrange(dplyr::across(c("speed", "dist")))

  # insert table
  tab <- insertTable(cdm = src, name = name, table = table)
  validateX(x = tab, name = name, fun = "insertTable")

  # check inserted table
  x <- tab |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::across(c("speed", "dist"))) |>
    unclass()
  attr(x, "tbl_source") <- NULL
  attr(x, "tbl_name") <- NULL
  if (!identical(x, unclass(table))) {
    cli::cli_abort("The inserted table was not the same than the original one.")
  }

  # compute inserted table
  tab <- tab |> compute(name = name, temporary = FALSE)
  validateX(x = tab, name = name, fun = "compute")

  # drop table
  if(!isTRUE(dropTable(cdm = src, name = name))) {
    cli::cli_abort("Source is invalid as table {name} couldn't be dropped.")
  }

  return(invisible(src))
}

validateX <- function(x, name, fun) {
  if (!identical(tableName(x), name)) {
    cli::cli_abort("table name is not correctly assigned in {fun}")
  }
  if (!"cdm_table" %in% class(x)) {
    cli::cli_abort("cdm_table class is not correctly assigned in {fun}")
  }
  return(invisible(TRUE))
}

#' @export
print.cdm_source <- function(x, ...) {
  cli::cli_inform(
    "This is a {sourceType(x)} cdm source"
  )
}

#' Get the source type of an object.
#'
#' @param x Object to know the source type.
#'
#' @return A character vector that defines the type of cdm_source.
#'
#' @export
#'
sourceType <- function(x) {
  UseMethod("sourceType")
}

#' @export
sourceType.cdm_source <- function(x) {
  attr(x, "source_type")
}

#' @export
sourceType.cdm_reference <- function(x) {
  x |> cdmSource() |> sourceType()
}

#' @export
sourceType.cdm_table <- function(x) {
  x |> cdmReference() |> sourceType()
}
