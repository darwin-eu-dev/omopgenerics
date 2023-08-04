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

#' CdmReference objects constructor.
#'
#' @param cohortTable Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetTable Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionTable Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCountTable Table with at least: cohort_definition_id,
#' number_subjects, number_records.
#' @param cohortName Name of the cdm_cohort object.
#'
#' @return A cdm_cohort object
#'
#' @export
#'
newCdmCohort <- function(cohortTable,
                         cohortSetTable = NULL,
                         cohortAttritionTable = NULL,
                         cohortCountTable = NULL,
                         cohortName = "cohort") {
  UseMethod("newCdmCohort")
}

#' `cdm_cohort` objects constructor.
#'
#' @param cohortTable Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetTable Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionTable Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCountTable Table with at least: cohort_definition_id,
#' number_subjects, number_records.
#' @param cohortName Name of the cdm_cohort object.
#'
#' @return A cdm_cohort object
#'
#' @export
#'
newCdmCohort.tbl <- function(cohortTable,
                             cohortSetTable = NULL,
                             cohortAttritionTable = NULL,
                             cohortCountTable = NULL,
                             cohortName = "cohort") {
  # initial input check
  checkInput(
    cohortTable = cohortTable, cohortSetTable = cohortSetTable,
    cohortAttritionTable = cohortAttritionTable,
    cohortCountTable = cohortCountTable
  )

  if (is.null(cohortSetTable)) {
    cohortSetTable <- cohortTable %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::mutate()
  }

  if (is.null(cohortCountTable)) {
    cohortCountTable <- cohortTable %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::summarise(
        number_subjects = dplyr::n_distinct(.data$subject_id),
        number_records = dplyr::n()
      ) %>%
      dplyr::right_join(
        cohortSetTable %>% dplyr::select("cohort_definition_id"),
        by = "cohort_definition_id"
      ) %>%
      dplyr::mutate(
        number_subjects = dplyr::coalesce(.data$number_subjects, 0),
        number_records = dplyr::coalesce(.data$number_records, 0)
      )
  }

  if (is.null(cohortAttritionTable)) {
    cohortAttritionTable <- cohortCountTable %>%
      dplyr::mutate(
        reason_id = 1, reason = "Qualifying initial events",
        excluded_subjects = 0, excluded_records = 0
      )
  }

  attr(cohortTable, "cohort_set") <- cohortSetTable
  attr(cohortTable, "cohort_attrition") <- cohortAttritionTable
  attr(cohortTable, "cohort_count") <- cohortCountTable
  class(cohortTable) <- c(
    "GeneratedCohortSet", # to be removed
    "cdm_cohort", class(cohortTable)[class(cohortTable) != "cdm_cohort"]
  )

  cohort <- validateCdmCohort(cohortTable)

  return(cohort)
}

#' To assess if an object is a valid `cdm_cohort`
#'
#' @param cohortTable Table to validate
#'
#' @export
#'
validateCdmCohort <- function(cohortTable) {
  UseMethod("validateCdmCohort")
}

#' To assess if an object is a valid `cdm_cohort`.
#'
#' @param cohortTable Table to validate
#'
#' @export
#'
validateCdmCohort.cdm_cohort <- function(cohortTable) {
  # class
  if (!("cdm_cohort" %in% class(cohortTable))) {
    cli::cli_abort("Object has not `cdm_cohort` class")
  }

  # attributes exist
  if (!all(
    c("cohort_set", "cohort_count", "cohort_attrition") %in%
    names(attributes(cohortTable))
  )) {
    cli::cli_abort(
      "`cohort_set`, `cohort_count`, `cohort_attrition` must be attributes of
      a cdm_cohort object."
    )
  }

  # get attributes
  cohort <- cohortTable
  cohort_set <- attr(cohortTable, "cohort_set")
  cohort_count <- attr(cohortTable, "cohort_count")
  cohort_attrition <- attr(cohortTable, "cohort_attrition")

  # assert columns
  checkColumns <- function(x, cols, nam) {
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of ",
        nam, " of a cdm_cohort object."
      ))
    }
    invisible(NULL)
  }
  checkColumns(
    cohortTable,
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"),
    "the cohort"
  )
  checkColumns(
    attr(cohortTable, "cohort_set"), c("cohort_definition_id", "cohort_name"),
    "the cohort_set"
  )
  checkColumns(
    attr(cohortTable, "cohort_count"),
    c("cohort_definition_id", "number_records", "number_subjects"),
    "the cohort_count"
  )
  checkColumns(
    attr(cohortTable, "cohort_attrition"),
    c("cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"),
    "the cohort_attrition"
  )

  # classes attributes
  clCohort <- cl(cohort)
  clCohortSet <- cl(cohort_set)
  clCohortCount <- cl(cohort_count)
  clCohortAttrition <- cl(cohort_attrition)
  if (!equal(clCohort, clCohortSet, clCohortCount, clCohortAttrition)) {
    cli::cli_abort(c(
      "class of cohort objects must be the same:",
      paste0("** class(cohort) = ", clCohort),
      paste0("** class(cohort_set) = ", clCohortSet),
      paste0("** class(cohort_attrition) = ", clCohortAttrition),
      paste0("** class(cohort_count) = ", clCohortCount)
    ))
  }

  # cohort_definition_id the same
  cdiCohort <- cdi(cohort)
  cdiCohortSet <- cdi(cohort_set)
  cdiCohortCount <- cdi(cohort_count)
  cdiCohortAttrition <- cdi(cohort_attrition)
  if (!equal(cdiCohort, cdiCohortSet, cdiCohortCount, cdiCohortAttrition)) {
    cli::cli_abort(c(
      "Present cohort_definition_id must be the same in all elements",
      paste0("** cohort :", cdiCohort),
      paste0("** cohort_set : ", cdiCohortSet),
      paste0("** cohort_attrition : ", cdiCohortCount),
      paste0("** cohort_count : ", cdiCohortAttrition)
    ))
  }

  # validate cohort overlap?
  # in observation?
  # column types

  # make correct order
  cohortTable <- cohortTable %>%
    dplyr::relocate(c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ))
  attr(cohortTable, "cohort_set") <- attr(cohortTable, "cohort_set") %>%
    dplyr::relocate(c("cohort_definition_id", "cohort_name"))
  attr(cohortTable, "cohort_count") <- attr(cohortTable, "cohort_count") %>%
    dplyr::relocate(
      c("cohort_definition_id", "number_records", "number_subjects")
    )
  attr(cohortTable, "cohort_attrition") <-
    attr(cohortTable, "cohort_attrition") %>%
    dplyr::relocate(c(
      "cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"
    ))

  return(invisible(cohortTable))
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
  paste0(class(x), collapse = ", ")
}
cdi <- function(x) {
  x %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort()
}

#' To collect a `cdm_cohort` object.
#'
#' @param x `cohort_cdm` object.
#' @param ... Not used (for compatibility).
#'
#' @export
collect.cdm_cohort <- function(x, ...) {
  x
}

