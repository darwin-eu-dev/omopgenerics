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

#' `cdm_reference` objects constructor
#'
#' @param tables List of tables that are part of the OMOP Common Data Model
#' reference.
#' @param cdmName Name of the cdm object.
#' @param cdmVersion Version of the cdm. Supported versions 5.3 and 5.4.
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmReference <- function(tables,
                         cdmName,
                         cdmVersion = NULL) {
  # inputs
  assertList(tables, named = TRUE, class = "cdm_table")
  assertCharacter(cdmName, length = 1)

  # get cdm version
  if (is.null(cdmVersion)) {
    cdmVersion <- getVersion(tables)
  }

  # get cdm source
  if (length(tables) < 2) {
    cli::cli_abort("At least person and observation_period should be provided in tables")
  }

  # constructor
  cdm <- newCdmReference(
    tables = tables, cdmName = cdmName, cdmVersion = cdmVersion,
    cdmSource = getTableSource(tables[[1]])
  )

  # validate
  cdm <- validateCdmReference(cdm)

  return(cdm)
}

getVersion <- function(cdm) {
  version <- tryCatch({
    version <- cdm[["cdm_source"]] |>
      dplyr::pull("cdm_version") |>
      as.character()
    if (tolower(substr(version, 1, 1)) == "v") {
      version <- substr(version, 2, nchar(version))
    }
    substr(version, 1, 3)
    },
    error = function(e) {"5.3"}
  )
  return(version)
}
newCdmReference <- function(tables, cdmName, cdmVersion, cdmSource) {
  structure(
    .Data = tables,
    cdm_name = cdmName,
    cdm_source = cdmSource,
    cdm_version = cdmVersion,
    class = "cdm_reference"
  )
}
validateCdmReference <- function(cdm) {
  # assert version
  assertChoice(cdmVersion(cdm), c("5.3", "5.4"), length = 1)

  # assert source
  assertClass(getCdmSource(cdm), "cdm_source")

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

  # validate omop tables
  omopTables <- omopTables(version = cdmVersion(cdm))
  omopTables <- omopTables[omopTables %in% names(cdm)]
  for (nm in omopTables) {
    if (nm %in% c("person", "observation_period")) {
      cdm[[nm]] <- omopTable(cdm[[nm]])
    } else {
      cdm[[nm]] <- tryCatch(
        expr = {omopTable(cdm[[nm]])},
        error = function(e){
          cli::cli_warn(c(
            "{nm} table not included in cdm because:", as.character(e)
          ))
          return(NULL)
        }
      )
    }
  }

  # validate achilles tables
  achillesTables <- achillesTables(version = cdmVersion(cdm))
  achillesTables <- achillesTables[achillesTables %in% names(cdm)]
  for (nm in achillesTables) {
    tryCatch(
      expr = {cdm[[nm]] <- achillesTable(cdm[[nm]])},
      error = function(e){
        cli::cli_warn(c(
          "{nm} table not included in cdm because:", as.character(e)
        ))
        cdm[[nm]] <- NULL
      }
    )
  }

  # not overlapping periods
  checkOverlapObservation(cdm$observation_period)

  return(invisible(cdm))
}
combine <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  paste0(paste0(x[-length(x)], collapse = ", "), " and ", x[length(x)])
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
checkOverlapObservation <- function(x, call = parent.frame()) {
  x <- x |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$observation_period_start_date) |>
    dplyr::mutate("next_observation_period_start_date" = dplyr::lead(
      .data$observation_period_start_date
    )) |>
    dplyr::filter(
      .data$observation_period_end_date >=
        .data$next_observation_period_start_date
    ) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x5 <- x |>
      dplyr::ungroup() |>
      utils::head(5) |>
      dplyr::glimpse() |>
      print(width = Inf) |>
      utils::capture.output()
    cli::cli_abort(
      message = c(
        "There is overlap between observation_periods, {nrow(x)} overlap{?s}
        detected{ifelse(nrow(x)<=5, ':', ' first 5:')}",
        x5[3:8]
      ),
      call = call
    )
  }
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
  if (all(!name %in% names(x))) return(NULL)
  if (length(name) > 1) {
    cli::cli_abort("You can only read one table of a cdm_reference.")
  }
  xraw <- unclass(x)
  tbl <- xraw[[name]]
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
  # check consistent naming of value
  if (!is.null(value)) {
    if (!"cdm_table" %in% class(value)) {
      call <- parent.frame()
      value <- tryCatch(
        expr = insertFromSource(cdm, value),
        error = function(e) {
          cli::cli_abort(
            message = c(
              "The object is not a cdm_table and it could not convert to a
              cdm_table.",
              as.character(e$message)
            ),
            call = call
          )
        }
      )
    }
    if (!identical(getTableSource(value), getCdmSource(cdm))) {
      cli::cli_abort("Table and cdm does not share a common source.")
    }
    remoteName <- getTableName(value)
    if (!is.na(remoteName) && name != remoteName) {
      cli::cli_abort(
        "You can't assign a table named {remoteName} to {name}. Please use
        compute to change table name."
      )
    }
    if (remoteName %in% omopTables()) {
      value <- value |> omopTable()
    }
    if (remoteName %in% achillesTables()) {
      value <- value |> achillesTable()
    }
  }

  if (length(name) > 1) {
    cli::cli_abort("You can only edit one table of a cdm_reference.")
  }

  # check name lowercase
  if (all(name != tolower(name))) {
    cli::cli_abort("name should be lowercase.")
  }

  # remove cdm_reference
  attr(value, "cdm_reference") <- NULL

  # assign
  cl <- class(cdm)
  cdm <- unclass(cdm)
  cdm[[name]] <- value
  class(cdm) <- cl

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
  type <- getCdmSource(x) |> getSourceType()
  name <- cdmName(x)
  nms <- names(x)
  classes <- lapply(names(x), function(nm) {
    cl <- base::class(x[[nm]])
    if ("omop_table" %in% cl) {
      return("omop_table")
    } else if ("cohort_table" %in% cl) {
      return("cohort_table")
    } else if ("achilles_table" %in% cl) {
      return("achilles_table")
    } else {
      return("cdm_table")
    }
  }) |>
    unlist()
  omop <- nms[classes == "omop_table"]
  cohort <- nms[classes == "cohort_table"]
  achilles <- nms[classes == "achilles_table"]
  other <- nms[classes == "cdm_table"]
  if (length(omop) == 0) omop <- "-"
  if (length(cohort) == 0) cohort <- "-"
  if (length(achilles) == 0) achilles <- "-"
  if (length(other) == 0) other <- "-"
  cli::cli_h1("# OMOP CDM reference ({type}) of {name}")
  cli::cli_li(paste("{.strong omop tables:}", paste(omop, collapse = ", ")))
  cli::cli_li(paste("{.strong cohort tables:}", paste(cohort, collapse = ", ")))
  cli::cli_li(paste("{.strong achilles tables:}", paste(achilles, collapse = ", ")))
  cli::cli_li(paste("{.strong other tables:}", paste(other, collapse = ", ")))
  invisible(x)
}

#' Retrieves the cdm reference into a local cdm.
#'
#' @param x A cdm_reference object.
#' @param ... For compatibility only, not used.
#'
#' @return A local cdm_reference.
#'
#' @export
#'
collect.cdm_reference <- function(x, ...) {
  name <- cdmName(x)
  x <- unclass(x)
  tables <- list()
  cohortTables <- list()
  for (nm in names(x)) {
    if (inherits(x[[nm]], "cohort_table")) {
      cohortTables[[nm]] <- x[[nm]] |> dplyr::collect()
    } else {
      tables[[nm]] <- x[[nm]] |> dplyr::collect()
    }
  }
  cdm <- cdmFromTables(
    tables = tables, cdmName = name, cohortTables = cohortTables
  )
  return(cdm)
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
achillesColumns <- function(table, version = "5.3") {
  assertVersion(version = version)
  assertTable(table = table, version = version, type = "achilles")
  requiredColumns(table = table, version = version, type = "achilles")
}

assertVersion <- function(version, call = parent.frame()) {
  assertChoice(x = version, choices = c("5.3", "5.4"), call = call)
}
assertTable <- function(table, version, type, call = parent.frame()) {
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

#' @export
str.cdm_reference <- function(object, ...) {
  src <- getCdmSource(object)
  mes <- glue::glue(
    "A cdm reference of {cdmName(object)} with {length(object)} tables: {paste0(names(object), collapse = ', ')}"
  )
  cat(mes, sep = "")
}

getCdmReference <- function(x) {
  attr(x, "cdm_reference")
}
getSourceType <- function(x) {
  attr(x, "source_type")
}
