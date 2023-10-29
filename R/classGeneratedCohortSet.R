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

#' `generated_cohort_set` objects constructor.
#'
#' @param cohortTable Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetTable Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionTable Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortName Name of the generated_cohort_set object.
#'
#' @return A generated_cohort_set object
#'
#' @export
#'
generatedCohortSet <- function(cohortTable,
                               cohortSetTable = NULL,
                               cohortAttritionTable = NULL,
                               cohortName = "cohort") {
  UseMethod("generatedCohortSet")
}

#' `generated_cohort_set` objects constructor.
#'
#' @param cohortTable Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetTable Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionTable Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortName Name of the generated_cohort_set object.
#'
#' @return A generated_cohort_set object
#'
#' @export
#'
generatedCohortSet.tbl <- function(cohortTable,
                                   cohortSetTable = NULL,
                                   cohortAttritionTable = NULL,
                                   cohortName = "cohort") {
  # initial input check
  checkInput(
    cohortTableTbl = cohortTable, cohortSetTableTbl = cohortSetTable,
    cohortAttritionTableTbl = cohortAttritionTable
  )

  # constructor
  cohort <- newGeneratedCohortSet(
    cohortTable = cohortTable,
    cohortSetTable = cohortSetTable,
    cohortAttritionTable = cohortAttritionTable,
    cohortName = cohortName
  )

  # validate
  cohort <- validateGeneratedCohortSet(cohort)

  return(cohort)
}

newGeneratedCohortSet <- function(cohortTable,
                                  cohortSetTable,
                                  cohortAttritionTable,
                                  cohortName) {
  if (is.null(cohortSetTable)) {
    cohortSetTable <- defaultCohortSet(cohortTable)
  }
  attr(cohortTable, "cohort_set") <- cohortSetTable

  if (is.null(cohortAttritionTable)) {
    cohortAttritionTable <- defaultCohortAttrition(cohortTable)
  }
  attr(cohortTable, "cohort_attrition") <- cohortAttritionTable

  newClass <- c("generated_cohort_set", "GeneratedCohortSet")
  class(cohortTable) <- c(
    newClass, class(cohortTable)[!(class(cohortTable) %in% newClass)]
  )

  attr(cohortTable, "tbl_name") <- cohortName

  return(cohortTable)
}
validateGeneratedCohortSet <- function(cohort) {
  # class
  if (!"generated_cohort_set" %in% class(cohort)) {
    cli::cli_abort("cohort has not generated_cohort_set class")
  }

  # attributes exist
  if (!all(c("cohort_set", "cohort_attrition", "tbl_name") %in%
           names(attributes(cohort)))) {
    cli::cli_abort(
      "`cohort_set` and `cohort_attrition` must be attributes of a generated_cohort_set
      object."
    )
  }

  # check name
  assertCharacter(attr(cohort, "tbl_name"), length = 1)

  # get attributes
  cohort_set <- attr(cohort, "cohort_set")
  cohort_attrition <- attr(cohort, "cohort_attrition")

  # assert columns
  checkColumns <- function(x, cols, nam) {
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of ",
        nam, " of a generated_cohort_set object."
      ))
    }
    invisible(NULL)
  }
  checkColumns(
    cohort,
    c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ),
    "the cohort"
  )
  checkColumns(
    cohort_set, c("cohort_definition_id", "cohort_name"), "the cohort_set"
  )
  checkColumns(
    cohort_attrition,
    c(
      "cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"
    ),
    "the cohort_attrition"
  )

  # classes attributes
  clCohort <- cl(cohort)
  clCohortSet <- cl(cohort_set)
  clCohortAttrition <- cl(cohort_attrition)
  if (!equal(clCohort, clCohortSet, clCohortAttrition)) {
    cli::cli_abort(c(
      "class of cohort objects must be the same:",
      paste0("** class(cohort) = ", clCohort),
      paste0("** class(cohort_set) = ", clCohortSet),
      paste0("** class(cohort_attrition) = ", clCohortAttrition)
    ))
  }

  # cohort_definition_id the same
  cdiCohort <- cdi(cohort)
  cdiCohortSet <- cdi(cohort_set)
  cdiCohortAttrition <- cdi(cohort_attrition)
  if (!equal(cdiCohort, cdiCohortSet, cdiCohortAttrition)) {
    cli::cli_abort(c(
      "Present cohort_definition_id must be the same in all elements",
      paste0("** cohort: ", cdiCohort),
      paste0("** cohort_set: ", cdiCohortSet),
      paste0("** cohort_attrition: ", cdiCohortAttrition)
    ))
  }

  # TODO
  # validate cohort overlap?
  # in observation?
  # column types

  # make correct order
  cohort <- cohort |>
    dplyr::relocate(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ))
  attr(cohort, "cohort_set") <- attr(cohort, "cohort_set") |>
    dplyr::relocate(c("cohort_definition_id", "cohort_name"))
  attr(cohort, "cohort_attrition") <- attr(cohort, "cohort_attrition") |>
    dplyr::relocate(c(
      "cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"
    ))

  invisible(cohort)
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
    sort() |>
    paste0(collapse = ", ")
}

#' To collect a `generated_cohort_set` object.
#'
#' @param x `generated_cohort_set` object.
#' @param ... Not used (for compatibility).
#'
#' @export
collect.generated_cohort_set <- function(x, ...) {
  attrib <- attributes(x)
  class(x) <- class(x)[class(x) != "generated_cohort_set"]
  x <- x |> dplyr::collect()
  attr(x, "cohort_set") <- dplyr::collect(attrib$cohort_set)
  attr(x, "cohort_attrition") <- dplyr::collect(attrib$cohort_attrition)
  class(x) <- c("generated_cohort_set", class(x))
  return(x)
}

defaultCohortSet <- function(cohort) {
  cohort |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::mutate("cohort_name" = paste0("cohort_", .data$cohort_definition_id))
}
defaultCohortAttrition <- function(cohort) {
  cohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    dplyr::left_join(
      attr(cohort, "cohort_set") |> dplyr::select("cohort_definition_id"),
      by = "cohort_definition_id"
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
    )
}

#' Get cohort settings from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet <- function(cohort) {
  checkInput(cohort = cohort)
  attr(cohort, "cohort_set") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get cohort counts from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount <- function(cohort) {
  checkInput(cohort = cohort)
  attr(cohort, "cohort_attrition") |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) |>
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get cohort attrition from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the attrition.
#'
#' @export
cohortAttrition <- function(cohort) {
  checkInput(cohort = cohort)
  attr(cohort, "cohort_attrition") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id)
}
