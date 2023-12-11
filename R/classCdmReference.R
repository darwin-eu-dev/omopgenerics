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
#' @param cdmSource Source of the cdm object.
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmReference <- function(cdmTables, cohortTables = list(), cdmName, cdmSource = NULL) {

  # inputs
  assertList(cdmTables, named = TRUE, class = "tbl")
  assertList(
    cohortTables, named = TRUE, class = c("generated_cohort_set", "tbl")
  )
  assertCharacter(cdmName, length = 1)
  assertClass(cdmSource, "cdm_source", null = TRUE)

  if (is.null(cdmSource)){
    if ("tbl_df" %in% class(cdmTables[[1]])) {
      cdmSource <- localSource(cdmName)
    } else {
      cli::cli_abort("cdmSource must be provided, create a cdmSource with the cdmSource() function.")
    }
  }

  # get cdm version
  cdmVersion <- getVersion(cdmTables)

  # constructor
  cdm <- newCdmReference(
    cdmTables = cdmTables, cohortTables = cohortTables, cdmName = cdmName,
    cdmVersion = cdmVersion, cdmSource = cdmSource
  )

  # validate
  cdm <- validateCdmReference(cdm)

  return(cdm)
}

getVersion <- function(cdm) {
  version <- tryCatch(
    cdm[["cdm_source"]] |> dplyr::pull("cdm_version"),
    error = function(e) {"5.3"}
  )
  return(version)
}
newCdmReference <- function(cdmTables, cohortTables, cdmName, cdmVersion, cdmSource) {
  cdm <- c(cdmTables, cohortTables)
  attr(cdm, "cdm_name") <- cdmName
  attr(cdm, "cdm_version") <- cdmVersion
  attr(cdm, "cdm_source") <- cdmSource
  class(cdm) <- "cdm_reference"
  return(cdm)
}
validateCdmReference <- function(cdm) {
  # assert name
  assertCharacter(attr(cdm, "cdm_name"), length = 1)

  # assert version
  cdmVersion <- attr(cdm, "cdm_version")
  assertChoice(cdmVersion, c("5.3", "5.4"), length = 1)

  # assert source
  assertClass(attr(cdm, "cdm_source"), "cdm_source")

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

  cdmTables <- standardTables(version = cdmVersion)

  # assertions for all the cdm tables
  for (nm in names(cdm)) {
    # assert lowercase columns
    cols <- colnames(cdm[[nm]])
    x <- cols[cols != tolower(cols)]
    if (length(x) > 0) {
      cli::cli_abort(
        "column{plural(x)} {combine(x)} in table {nm} should be lowercase"
      )
    }

    # assert columnames match version
    if (nm %in% cdmTables) {
      cols <- requiredTableColumns(table = nm, version = cdmVersion)
      checkColumnsCdm(cdm[[nm]], nm, cols)
    } else if ("generated_cohort_set" %in% class(cdm[[nm]])) {
      cohort <- cdm[[nm]]
      cols <- requiredTableColumns(table = "cohort", version = cdmVersion)
      checkColumnsCdm(cohort, nm, cols)
      cols <- requiredTableColumns(table = "cohort_set", version = cdmVersion)
      checkColumnsCdm(set(cohort), paste0(nm, "_set"), cols)
      cols <- requiredTableColumns(table = "cohort_attrition", version = cdmVersion)
      checkColumnsCdm(attrition(cohort), paste0(nm, "_attrition"), cols)
    }
  }

  # TODO
  # assertions for cohort tables

  return(invisible(cdm))
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
checkColumnsCdm <- function(table, nm, required, call = parent.frame()) {
  columns <- colnames(table)

  # check required
  x <- required[!required %in% columns]
  if (length(x) > 0) {
    cli::cli_abort(
      "{combine(x)} {verb(x)} not present in table {nm}", call = call
    )
  }

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
  UseMethod("cdmName")
}

#' Name of a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#'
#' @return Name of the cdm_reference.
#'
#' @export
#'
cdmName.cdm_reference <- function(cdm) {
  attr(cdm, "cdm_name")
}

#' Version of a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#'
#' @return Version of the cdm_reference.
#'
#' @export
#'
cdmVersion <- function(cdm) {
  UseMethod("cdmVersion")
}

#' Version of a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#'
#' @return Version of the cdm_reference.
#'
#' @export
#'
cdmVersion.cdm_reference <- function(cdm) {
  attr(cdm, "cdm_name")
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
  x_raw <- unclass(x)
  tbl <- x_raw[[name]]
  attr(tbl, "cdm_reference") <- x
  tbl <- cdmTable(tbl)
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
  if (!is.null(value)) {
    if (!identical(getCdmSource(value), getCdmSource(cdm))) {
      cli::cli_abort("Table and cdm does not share a common source, please insert table to the cdm with insertTable")
    }
    remoteName <- attr(value, "tbl_name")
    if (is.null(remoteName)) {
      cli::cli_abort("The table that you are tying to assign does not have a name.")
    }
    if (!is.na(remoteName) && name != remoteName) {
      cli::cli_abort("You can't assign a table named {remoteName} to {name}. Please use compute to change table name.")
    }
  }
  attr(value, "cdm_reference") <- NULL
  originalClass <- class(cdm)
  cdm <- unclass(cdm)
  cdm[[name]] <- value
  class(cdm) <- originalClass
  return(cdm)
}

#' Print a CDM reference object
#'
#' @param x A cdm_reference object
#' @param ... Included for compatibility with generic. Not used.
#'
#' @return Invisibly returns the input
#' @export
print.cdm_reference <- function(x, ...) {
  src <- getCdmSource(x)
  type <- attr(src, "source_type")
  ref <- attr(src, "source_name")
  cli::cat_line(glue::glue("# OMOP CDM reference ({type}) of {ref}"))
  cli::cat_line("")
  cli::cat_line(paste("Tables:", paste(names(x), collapse = ", ")))
  invisible(x)
}

#' Standard tables that a cdm reference can contain in the OMOP Common Data
#' Model.
#'
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Standard tables
#'
#' @export
#'
standardTables <- function(version = "5.3") {
  # check inputs
  assertChoice(version, c("5.3", "5.4"))

  # filter
  tables <- fieldsTables$cdm_table_name[
    grepl(version, fieldsTables$cdm_version)
  ] |>
    unique()

  return(tables)
}

#' Required columns that the standard tables in the OMOP Common Data Model must
#' have.
#'
#' @param table Table to see required columns.
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Required columns
#'
#' @export
#'
requiredTableColumns <- function(table, version = "5.3") {
  # check input
  assertChoice(x = version, choices = c("5.3", "5.4"))
  assertChoice(x = table, choices = standardTables(version = version))

  # filter
  columns <- fieldsTables$cdm_field_name[
    grepl(version, fieldsTables$cdm_version) &
      fieldsTables$cdm_table_name == table &
      fieldsTables$is_required == TRUE
  ]

  return(columns)
}

#' Required columns for a generated cohort set.
#'
#' @param table Either `cohort`, `cohort_set` or `cohort_attrition`
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Required columns
#'
#' @export
#'
requiredCohortColumns <- function(table, version = "5.3") {
  # check input
  assertChoice(x = version, choices = c("5.3", "5.4"))
  assertChoice(x = table, choices = unique(fieldsCohorts$cdm_table_name))

  # filter
  columns <- fieldsCohorts$cdm_field_name[
    grepl(version, fieldsCohorts$cdm_version) &
      fieldsCohorts$cdm_table_name == table &
      fieldsCohorts$is_required == TRUE
  ]

  return(columns)
}
