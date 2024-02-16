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

#' `cohort_table` objects constructor.
#'
#' @param table cdm_table object with at least: cohort_definition_id,
#' subject_id, cohort_start_date, cohort_end_date.
#' @param cohortSetRef Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionRef Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param .softValidation Whether to perform a soft validation of consistency.
#'
#' @return A cohort_table object
#'
#' @export
#'
newCohortTable <- function(table,
                           cohortSetRef = attr(table, "cohort_set"),
                           cohortAttritionRef = attr(table, "cohort_attrition"),
                           .softValidation = FALSE) {
  # initial checks
  assertClass(table, "cdm_table")
  assertChoice(.softValidation, choices = c(TRUE, FALSE), length = 1)

  # populate
  cohortSetRef <- populateCohortSet(table, cohortSetRef)
  cohortAttritionRef <- populateCohortAttrition(
    table, cohortSetRef, cohortAttritionRef
  )

  # constructor
  cohort <- constructGeneratedCohortSet(
    table = table,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef
  )

  # validate
  cohort <- validateGeneratedCohortSet(cohort, soft = .softValidation)

  # return
  return(cohort)
}

#' To collect a `cohort_table` object.
#'
#' @param x `cohort_table` object.
#' @param ... Not used (for compatibility).
#'
#' @return A data frame with the `cohort_table`
#'
#' @export
#'
#' @importFrom dplyr collect
#'
collect.cohort_table <- function(x, ...) {
  x <- removeClass(x, "cohort_table")
  y <- x |> dplyr::collect()
  attr(y, "cohort_set") <- attr(x, "cohort_set") |> dplyr::collect()
  attr(y, "cohort_attrition") <- attr(x, "cohort_attrition") |> dplyr::collect()
  return(y)
}

constructGeneratedCohortSet <- function(table,
                                        cohortSetRef,
                                        cohortAttritionRef) {
  table <- structure(
    .Data = table,
    "cohort_set" = noReference(cohortSetRef),
    "cohort_attrition" = noReference(cohortAttritionRef)
  ) |>
    addClass(c("cohort_table", "GeneratedCohortSet"))
  return(table)
}
validateGeneratedCohortSet <- function(cohort, soft = FALSE) {
  # get attributes
  cohort_set <- attr(cohort, "cohort_set")
  cohort_attrition <- attr(cohort, "cohort_attrition")

  # assertClass
  assertClass(cohort, "cdm_table")
  assertClass(cohort_set, "cdm_table")
  assertClass(cohort_attrition, "cdm_table")

  # check cdm reference
  if (!"cdm_reference" %in% names(attributes(cohort))) {
    cli::cli_abort("cohort must be part of a cdm_reference")
  }

  # check name
  assertCharacter(tableName(cohort), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_set), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_attrition), length = 1, na = TRUE)
  consistentNaming(
    cohortName = tableName(cohort),
    cohortSetName = tableName(cohort_set),
    cohortAttritionName = tableName(cohort_attrition)
  )

  # check source
  srcCohort <- tableSource(cohort)
  srcCohortSet <- tableSource(cohort_set)
  srcCohortAttrition <- tableSource(cohort_attrition)
  if (!equal(srcCohort, srcCohortSet, srcCohortAttrition)) {
    cli::cli_abort(
      "The source must be the same for cohort, cohort_set and cohort_attrition."
    )
  }

  # assert columns
  checkColumnsCohort <- function(x, nam) {
    cols <- cohortColumns(nam)
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of the ",
        nam, " of a cohort_table object."
      ))
    }
    invisible(NULL)
  }
  checkColumnsCohort(cohort, "cohort")
  checkColumnsCohort(cohort_set, "cohort_set")
  checkColumnsCohort(cohort_attrition, "cohort_attrition")

  # cohort_definition_id is coherent
  cdiCohort <- cdi(cohort)
  cdiCohortSet <- cdi(cohort_set)
  cdiCohortAttrition <- cdi(cohort_attrition)
  if (!all(cdiCohortSet == cdiCohortAttrition)) {
    cli::cli_abort(c(
      "Present cohort_definition_id must be the same:",
      "*" = "cohort_set: {cdiCohortSet}",
      "*" = "cohort_attrition: {cdiCohortAttrition}"
    ))
  }
  if (!all(cdiCohort %in% cdiCohortSet)) {
    cli::cli_abort(c(
      "There are cohort_definition_id that appear in cohort and not in cohort_set:",
      "*" = "cohort: {paste0(cdiCohort, collapse = ', ')}",
      "*" = "cohort_set: {paste0(cdiCohortSet, collapse = ', ')}"
    ))
  }

  # cohort_name column
  cohortNames <- cohort_set |> dplyr::pull("cohort_name")
  if (length(cohortNames) != length(unique(cohortNames))) {
    cli::cli_abort("cohort_name in the cohort_set must be unique")
  }
  notSnake <- cohortNames[!isSnakeCase(cohortNames)]
  if (length(notSnake)) {
    oldName <- notSnake
    newName <- snakecase::to_snake_case(notSnake)
    x <- paste0(oldName, " -> ", newName)
    names(x) <- rep("*", length(x))
    cli::cli_warn(c(
      "cohort_name must be snake case, the following cohorts will be renamed:",
      x
    ))
    nameChange <- dplyr::tibble(
      "xyz_cohort_name" = newName, "cohort_name" = oldName
    )
    attr(cohort, "cohort_set") <- attr(cohort, "cohort_set") |>
      dplyr::left_join(nameChange, by = "cohort_name", copy = TRUE) |>
      dplyr::mutate("cohort_name" = dplyr::if_else(
        is.na(.data$xyz_cohort_name), .data$cohort_name, .data$xyz_cohort_name
      )) |>
      dplyr::select(-"xyz_cohort_name") |>
      dplyr::compute(
        name = paste0(tableName(cohort), "_set"), temporary = FALSE,
        overwrite = TRUE
      )
  }

  # make correct order
  cohort <- cohort |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort")))
  attr(cohort, "cohort_set") <- attr(cohort, "cohort_set") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_set")))
  attr(cohort, "cohort_attrition") <- attr(cohort, "cohort_attrition") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_attrition")))

  if (!soft) {
    # check start before end
    checkStartEnd(cohort)

    # check NA
    checkNaCohort(cohort)

    # check within observation period
    checkObservationPeriod(cohort)

    # check overlap
    checkOverlap(cohort)
  }

  return(cohort)
}
equal <- function(...) {
  x <- list(...)
  flag <- TRUE
  for (k in 2:length(x)) {
    flag <- flag & all(x[[1]]==x[[k]])
  }
  return(flag)
}
cl <- function(x) {
  x <- class(x)
  x <- x[!grepl(pattern = "cohort", x = x, ignore.case = TRUE)]
  paste0(x, collapse = ", ")
}
cdi <- function(x) {
  x |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::pull() |>
    sort()
}
defaultCohortSet <- function(cohort) {
  cohortName <- tableName(cohort)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_set"))
  cohort |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "cohort_name" = paste0("cohort_", as.character(.data$cohort_definition_id))
    ) |>
    collect()
}
defaultCohortAttrition <- function(cohort, set) {
  cohortName <- tableName(cohort)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_attrition"))
  x <- cohort |>
    group_by(.data$cohort_definition_id) |>
    summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    dplyr::right_join(
      set |> dplyr::select("cohort_definition_id"),
      by = "cohort_definition_id",
      copy = TRUE
    ) |>
    dplyr::mutate(
      "number_records" = dplyr::if_else(
        is.na(.data$number_records), 0, .data$number_records
      ),
      "number_subjects" = dplyr::if_else(
        is.na(.data$number_subjects), 0, .data$number_subjects
      ),
      "reason_id" = 1,
      "reason" = "Initial qualifying events",
      "excluded_records" = 0,
      "excluded_subjects" = 0
    ) |>
    collect()
  return(x)
}
checkStartEnd <- function(cohort, call = parent.frame()) {
  x <- cohort |>
    dplyr::filter(.data$cohort_end_date < .data$cohort_start_date) |>
    dplyr::collect()
  if (nrow(x) > 0){
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(cohortColumns("cohort"))) |>
      utils::head(5) |>
      dplyr::glimpse() |>
      print(width = Inf) |>
      utils::capture.output()
    cli::cli_abort(
      message = c(
        "cohort_start_date must be <= tham cohort_end_date. There are {nrow(x)}
        entries where cohort_end_date < cohort_start_date
        {ifelse(nrow(x)<=5, ':', ' first 5:')}",
        x5[3:7]
      ),
      call = call
    )
  }
  return(invisible(TRUE))
}
checkOverlap <- function(cohort, call = parent.frame()) {
  x <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(
      "next_cohort_start_date" = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::filter(.data$cohort_end_date >= .data$next_cohort_start_date) |>
    dplyr::collect()
  if (nrow(x) > 0){
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date", "next_cohort_start_date"
      ) |>
      utils::head(5) |>
      dplyr::glimpse() |>
      print(width = Inf) |>
      utils::capture.output()
    cli::cli_abort(
      message = c(
        "There is overlap between entries in the cohort, {nrow(x)} overlap{?s}
        detected{ifelse(nrow(x)<=5, ':', ' first 5:')}",
        x5[3:7]
      ),
      call = call
    )
  }
  return(invisible(TRUE))
}
checkNaCohort <- function(cohort, call = parent.frame()) {
  x <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ is.na(.x))) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x <- x |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(is.na(.data$value)) |>
      dplyr::pull("name") |>
      unique() |>
      paste0(collapse = ", ")
    cli::cli_abort(
      "Cohort can't have NA values, there are NA values in the following
      columns: {x}",
      call = call
    )
  }
  return(invisible(TRUE))
}
checkObservationPeriod <- function(cohort, call = parent.frame()) {
  cdm <- attr(cohort, "cdm_reference")
  x <- cohort |>
    dplyr::anti_join(
      cohort |>
        dplyr::select(dplyr::all_of(cohortColumns("cohort"))) |>
        dplyr::left_join(
          cdm[["observation_period"]] |>
            dplyr::select(
              "subject_id" = "person_id", "observation_period_start_date",
              "observation_period_end_date"
            ),
          by = "subject_id"
        ) |>
        dplyr::filter(
          .data$cohort_start_date >= .data$observation_period_start_date &
            .data$cohort_start_date <= .data$observation_period_end_date &
            .data$cohort_end_date >= .data$observation_period_start_date &
            .data$cohort_end_date <= .data$observation_period_end_date
        ),
      by = cohortColumns("cohort")
    ) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    cli::cli_abort(
      message = "{nrow(x)} observation{?s} outside observation period.",
      call = call
    )
  }
  return(invisible(TRUE))
}
consistentNaming <- function(cohortName, cohortSetName, cohortAttritionName) {
  if (is.na(cohortName)) {
    if (!is.na(cohortSetName) | !is.na(cohortAttritionName)) {
      cli::cli_abort("cohort is a temp table, cohort_set and cohort_attrition should be a temp table too")
    }
  } else {
    errorMessage <- character()
    if (cohortSetName != paste0(cohortName, "_set")) {
      errorMessage <- c(errorMessage, "cohort_set name must be {paste0(cohortName, '_set')} but is {cohortSetName}")
    }
    if (cohortAttritionName != paste0(cohortName, "_attrition")) {
      errorMessage <- c(errorMessage, "cohort_attrition name must be {paste0(cohortName, '_attrition')} but is {cohortAttritionName}")
    }
    if (length(errorMessage) > 0) {
      cli::cli_abort(errorMessage)
    }
  }
  return(invisible(TRUE))
}
populateCohortSet <- function(table, cohortSetRef) {
  if (is.null(cohortSetRef)) {
    cohortSetRef <- defaultCohortSet(table)
  } else {
    cohortSetRef <- cohortSetRef |> dplyr::collect()
  }
  cohortName <- tableName(table)
  assertClass(cohortSetRef, "data.frame", null = TRUE)
  cohortSetRef <- dplyr::as_tibble(cohortSetRef)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_set"))
  cohortSetRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortSetRef,
    overwrite = TRUE
  )
  return(cohortSetRef)
}
populateCohortAttrition <- function(table, cohortSetRef, cohortAttritionRef) {
  if (is.null(cohortAttritionRef)) {
    cohortAttritionRef <- defaultCohortAttrition(table, cohortSetRef)
  } else {
    cohortAttritionRef <- cohortAttritionRef |> dplyr::collect()
  }
  cohortName <- tableName(table)
  assertClass(cohortAttritionRef, "data.frame", null = TRUE)
  cohortAttritionRef <- dplyr::as_tibble(cohortAttritionRef)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_attrition"))
  cohortAttritionRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortAttritionRef,
    overwrite = TRUE
  )
  return(cohortAttritionRef)
}

#' Create an empty cohort_table object
#'
#' @param name Name of the table to create.
#' @param cdm A cdm_reference to create the table.
#'
#' @export
#'
#' @return The cdm_reference with an empty cohort table
#'
emptyCohortTable <- function(name, cdm) {
  assertCharacter(name, length = 1)
  assertClass(cdm, "cdm_reference")
  table <- fieldsTables |>
    dplyr::filter(
      .data$cdm_table_name == "cohort" &
        .data$type == "cohort" &
        grepl(cdmVersion(cdm), .data$cdm_version)
    ) |>
    emptyTable()
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = FALSE)
  cdm[[name]] <- newCohortTable(cdm[[name]])
  return(cdm)
}

emptyTable <- function(fields) {
  lapply(fields$cdm_datatype, getEmptyField) |>
    rlang::set_names(fields$cdm_field_name) |>
    dplyr::as_tibble()
}
getEmptyField <- function(datatype) {
  datatype[grepl("varchar", datatype)] <- "varchar"
  empty <- switch(
    datatype,
    "integer" = integer(),
    "datetime" = as.Date(integer()),
    "date" = as.Date(integer()),
    "float" = numeric(),
    "varchar" = character(),
    "logical" = logical()
  )
  return(empty)
}

