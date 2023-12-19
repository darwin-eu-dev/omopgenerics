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
#' @param achillesTables List of tables that contain achilles.
#' @param cdmName Name of the cdm.
#' @param cdmSource Source of the cdm object.
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmReference <- function(cdmTables, cohortTables = list(), achillesTables = list(), cdmName, cdmSource = NULL) {

  # inputs
  assertList(cdmTables, named = TRUE)
  assertList(cohortTables, named = TRUE)
  assertList(achillesTables, named = TRUE)
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
    cdmTables = cdmTables, achillesTables = achillesTables, cdmName = cdmName,
    cdmVersion = cdmVersion, cdmSource = cdmSource
  )

  # validate
  cdm <- validateCdmReference(cdm)

  # add cohort tables
  for (nm in names(cohortTables)) {
    x <- cohortTables[[nm]]
    attr(x, "cdm_reference") <- cdm
    attr(x, "tbl_name") <- nm
    x <- cdmTable(x)
    if ("generated_cohort_set" %in% class(x)) {
      cdm[[nm]] <- x
    } else {
      cdm[[nm]] <- generatedCohortSet(
        cohortRef = x,
        cohortSetRef = attr(x, "cohort_set"),
        cohortAttritionRef = attr(x, "cohort_attrition"),
        overwrite = FALSE
      )
    }
  }

  return(cdm)
}

getVersion <- function(cdm) {
  version <- tryCatch(
    cdm[["cdm_source"]] |> dplyr::pull("cdm_version"),
    error = function(e) {"5.3"}
  )
  return(version)
}
newCdmReference <- function(cdmTables, achillesTables, cdmName, cdmVersion, cdmSource) {
  cdm <- list()
  class(cdm) <- "cdm_reference"
  for (nm in names(cdmTables)) {
    cdm <- appendToCdm(cdm, cdmTables[[nm]], nm)
  }
  for (nm in names(achillesTables)) {
    cdm <- appendToCdm(cdm, achillesTables[[nm]], nm)
  }
  attr(cdm, "cdm_source") <- cdmSource
  attr(cdm, "cdm_name") <- cdmName
  attr(cdm, "cdm_version") <- cdmVersion
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

  cdmTables <- omopTables(version = cdmVersion)

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
      cols <- omopColumns(table = nm, version = cdmVersion)
      checkColumnsCdm(cdm[[nm]], nm, cols)
    } else if ("generated_cohort_set" %in% class(cdm[[nm]])) {
      cohort <- cdm[[nm]]
      cols <- omopColumns(table = "cohort", version = cdmVersion)
      checkColumnsCdm(cohort, nm, cols)
      cols <- omopColumns(table = "cohort_set", version = cdmVersion)
      checkColumnsCdm(settings(cohort), paste0(nm, "_set"), cols)
      cols <- omopColumns(table = "cohort_attrition", version = cdmVersion)
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
  assertClass(cdm, "cdm_reference")
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
  assertClass(cdm, "cdm_reference")
  attr(cdm, "cdm_version")
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
  if (!name %in% names(x)) return(NULL)
  tbl <- getFromCdm(x, name)
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
  if (is.null(value)) {
    cdm <- deleteCdmElement(cdm, name)
    return(cdm)
  } else {
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
  cdm <- appendToCdm(cdm, value, name)
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
omopTables <- function(version = "5.3") {
  assertVersion(version = version)
  tableChoice(version = version, type = "cdm_table")
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
omopColumns <- function(table, version = "5.3") {
  assertVersion(version = version)
  assertTable(table = table, version = version, type = "cdm_table")
  requiredColumns(table = table, version = version, type = "cdm_table")
}

#' Cohort tables that a cdm reference can contain in the OMOP Common Data
#' Model.
#'
#' @param version Version of the OMOP Common Data Model.
#'
#' @return cohort tables
#'
#' @export
#'
cohortTables <- function(version = "5.3") {
  assertVersion(version = version)
  tableChoice(version = version, type = "cohort")
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
cohortColumns <- function(table, version = "5.3") {
  assertVersion(version = version)
  assertTable(table = table, version = version, type = "cohort")
  requiredColumns(table = table, version = version, type = "cohort")
}

#' Tables containing the results of achilles analyses
#'
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Names of tables returned by achilles analyses
#' @export
#'
#' @examples
achillesTables <- function(version = "5.3"){
  assertVersion(version = version)
  tableChoice(version = version, type = "achilles")
}

#' Required columns for achilles result tables
#'
#' @param table Table to see required columns.
#' @param version Version of the OMOP Common Data Model.
#'
#' @return Names of columns for achilles result tables
#' @export
#'
#' @examples
achillesColumns <- function(table, version = "5.3") {
  assertVersion(version = version)
  assertTable(table = table, version = version, type = "achilles")
  requiredColumns(table = table, version = version, type = "achilles")
}

assertVersion <- function(version, call = parent.env()) {
  assertChoice(x = version, choices = c("5.3", "5.4"), call = call)
}
assertTable <- function(table, version, type, call = parent.env()) {
  assertChoice(x = table, choices = tableChoice(version, type), call = call)
}
tableChoice <- function(version, type) {
  fieldsTables$cdm_table_name[
    grepl(version, fieldsTables$cdm_version) &
      fieldsTables$type == type
  ] |>
    unique()
}
requiredColumns <- function(table, version, type) {
  fieldsTables$cdm_field_name[
    grepl(version, fieldsTables$cdm_version) &
      fieldsTables$cdm_table_name == table &
      fieldsTables$is_required == TRUE &
      fieldsTables$type == type
  ]
}

appendToCdm <- function(cdm, x, name) {
  cl <- class(cdm)
  cdm <- unclass(cdm)
  if (inherits(x, "generated_cohort_set")) {
    cdm <- appendSet(cdm, x, name)
    cdm <- appendAttrition(cdm, x, name)
  }
  cdm <- appendTable(cdm, x, name)
  class(cdm) <- cl
  return(cdm)
}
appendTable <- function(cdm, x, name) {
  attr(x, "tbl_name") <- name
  cdm[[name]] <- unclass(x)
  attr(cdm, "classes") <- append(
    x = attr(cdm, "classes"),
    values = list(class(x)) |> rlang::set_names(name)
  )
  return(cdm)
}
appendSet <- function(cdm, x, name) {
  set <- attr(x, "cohort_set")
  attr(set, "tbl_name") <- paste0(name, "_set")
  attr(cdm, "cohort_set") <- append(
    x = attr(cdm, "cohort_set"),
    values = list(unclass(set)) |> rlang::set_names(name)
  )
  attr(cdm, "cohort_set_classes") <- append(
    x = attr(cdm, "cohort_set_classes"),
    values = list(class(set)) |> rlang::set_names(name)
  )
  return(cdm)
}
appendAttrition <- function(cdm, x, name) {
  attri <- attr(x, "cohort_attrition")
  attr(attri, "tbl_name") <- paste0(name, "_attrition")
  attr(cdm, "cohort_attrition") <- append(
    x = attr(cdm, "cohort_attrition"),
    values = list(unclass(attri)) |> rlang::set_names(name)
  )
  attr(cdm, "cohort_attrition_classes") <- append(
    x = attr(cdm, "cohort_attrition_classes"),
    values = list(class(attri)) |> rlang::set_names(name)
  )
  return(cdm)
}
getFromCdm <- function(cdm, name) {
  cdm <- unclass(cdm)
  x <- getTable(cdm, name)
  if (inherits(x, "generated_cohort_set")) {
    attr(x, "cohort_set") <- getSet(cdm, name)
    attr(x, "cohort_attrition") <- getAttrition(cdm, name)
  }
  return(x)
}
getTable <- function(cdm, name) {
  x <- cdm[[name]]
  class(x) <- attr(cdm, "classes")[[name]]
  return(x)
}
getSet <- function(cdm, name) {
  x <- attr(cdm, "cohort_set")[[name]]
  class(x) <- attr(cdm, "cohort_set_classes")[[name]]
  return(x)
}
getAttrition <- function(cdm, name) {
  x <- attr(cdm, "cohort_attrition")[[name]]
  class(x) <- attr(cdm, "cohort_attrition_classes")[[name]]
  return(x)
}
deleteCdmElement <- function(cdm, name) {
  x <- cdm[[name]]
  cl <- class(cdm)
  cdm <- unclass(cdm)
  if (inherits(x, "generated_cohort_set")) {
    attr(cdm, "cohort_set")[[name]] <- NULL
    attr(cdm, "cohort_set_classes")[[name]] <- NULL
    attr(cdm, "cohort_attrition")[[name]] <- NULL
    attr(cdm, "cohort_attrition_classes")[[name]] <- NULL
  }
  cdm[[name]] <- NULL
  attr(cdm, "classes")[[name]] <- NULL
  class(cdm) <- cl
  return(cdm)
}
