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
#' @param cdmTables List of tables that contains a reference to an OMOP Common
#' Data Model.
#' @param cdmName Name of the cdm.
#' @param cdmVersion Version of the cdm ("5.3" or "5.4").
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmReference <- function(cdmTables, cdmName, cdmVersion) {

  # inputs
  assertList(cdmTables, named = TRUE, class = "tbl")
  assertChoice(cdmVersion, c("5.3", "5.4"), length = 1)
  assertCharacter(cdmName, length = 1)

  # constructor
  cdm <- newCdmReference(
    cdmTables = cdmTables, cdmName = cdmName, cdmVersion = cdmVersion
  )

  # validate
  cdm <- validateCdmReference(cdm)

  return(cdm)
}

newCdmReference <- function(cdmTables, cdmName, cdmVersion) {
  attr(cdmTables, "cdm_name") <- cdmName
  attr(cdmTables, "cdm_version") <- cdmVersion
  class(cdmTables) <- "cdm_reference"
  return(cdmTables)
}
validateCdmReference <- function(cdm) {
  # assert class
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort("A cdm_reference object must have class cdm_reference.")
  }

  # assert name
  assertCharacter(attr(cdm, "cdm_name"), length = 1)

  # assert version
  cdmVersion <- attr(cdm, "cdm_version")
  assertChoice(cdmVersion, c("5.3", "5.4"), length = 1)

  # assert lowercase names
  x <- names(cdm)[names(cdm) != tolower(names(cdm))]
  if (length(x) > 0) {
    cli::cli_abort(
      "table names should be lower case; {combine(x)} {verb(x)} not."
    )
  }

  # assert compulsory tables
  compulsoryTables <- c("person", "observation_period")
  x <- compulsoryTables[!compulsoryTables %in% names(cdm)]
  if (length(x) > 0) {
    cli::cli_abort("{combine(x)} {verb(x)} not included in the cdm object")
  }

  cdmTables <- fieldsTables$cdmTableName |> unique()
  cdmTables <- cdmTables[cdmTables %in% names(cdm)]

  # assertions for all the cdm tables
  for (nm in cdmTables) {
    # assert lowercase columns
    cols <- colnames(cdm[[nm]])
    x <- cols[cols != tolower(cols)]
    if (length(x) > 0) {
      cli::cli_abort(
        "column{plural(x)} {combine(x)} in table {nm} should be lowercase"
      )
    }

    # assert columnames match version
    specifications <- fieldsTables |>
      dplyr::filter(
        grepl(.env$cdmVersion, .data$cdm_version) &
          .data$cdmTableName == .env$nm
      ) |>
      dplyr::select(
        "colname" = "cdmFieldName", "required" = "isRequired",
        "type" = "cdmDatatype"
      )
    checkColumns(cdm, nm, specifications)

  }

  # assertions for cohort tables
  cohortTables <- names(cdm)[!names(cdm) %in% cdmTables]
  x <- character()
  for (nm in cohortTables) {
    if (!"generated_cohort_set" %in% class(cdm[[nm]])) {
      x <- c(x, nm)
    }
  }
  if (length(x) > 0) {
    cli::cli_abort("Table{plural(x)} {verb(x)} not standard cdm table{plural(x)} or cohort{plural(x)}")
  }

  return(invisible(cdm))
}
isLowerCase <- function(x) {
  all(x == tolower(x))
}
combine <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  paste0(paste0(x[length(x) - 1], ", "), " and ", x[length(x)])
}
verb <- function(x) {
  ifelse(length(x) == 1, "is", "are")
}
plural <- function(x) {
  ifelse(length(x) == 1, "", "s")
}
checkColumns <- function(cdm, nm, specifications, call = parent.frame()) {
  columns <- colnames(cdm[[nm]])

  # check required
  required <- specifications |>
    dplyr::filter(.data$required == TRUE) |>
    dplyr::pull("colname")
  x <- required[!required %in% columns]
  if (length(x) > 0) {
    cli::cli_abort(
      "{combine(x)} {verb(x)} not present in table {nm}", call = call
    )
  }

  # check extra
  allCols <- specifications |> dplyr::pull("colname")
  x <- columns[!columns %in% allCols]
  if (length(x) > 0) {
    cli::cli_abort("Column{plural(x)} {verb(x)} not allowed in table {nm}")
  }

  # TODO
  # check type

  return(invisible(TRUE))
}


#' Name of a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#'
#' @return Name of the cdm_reference.
#'
#' @export
#'
cdmName <- function(cdm) {
  checkInput(cdm = cdm)
  attr(cdm, "cdm_name")
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

#' @export
`[[<-.cdm_reference` <- function(obj, name, value) {
  x <- class(obj)
  attr(value, "cdm_reference") <- NULL
  obj <- unclass(obj)
  obj[[name]] <- value
  class(obj) <- x
  return(obj)
}

#' @export
`$<-.cdm_reference` <- function(obj, name, value) {
  x <- class(obj)
  attr(value, "cdm_reference") <- NULL
  obj <- unclass(obj)
  obj[[name]] <- value
  class(obj) <- x
  return(obj)
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
