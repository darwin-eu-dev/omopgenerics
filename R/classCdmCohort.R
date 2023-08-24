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
    cohortSetTable <- createCohortSet(cohortTable)
  }
  attr(cohortTable, "cohort_set") <- cohortSetTable

  if (is.null(cohortCountTable)) {
    cohortCountTable <- createCohortCount(cohortTable)
  }
  attr(cohortTable, "cohort_count") <- cohortCountTable

  if (is.null(cohortAttritionTable)) {
    cohortAttritionTable <- createCohortAttrition(cohortTable)
  }
  attr(cohortTable, "cohort_attrition") <- cohortAttritionTable

  class(cohortTable) <- c(
    "GeneratedCohortSet", # to be removed
    "cdm_cohort", class(cohortTable)[class(cohortTable) != "cdm_cohort"]
  )

  cohort <- validateCdmCohort(cohortTable)

  return(cohort)
}

#' To assess if an object is a valid `cdm_cohort`
#'
#' @param cohortTable `cdm_cohort` to validate.
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
  # attributes exist
  if (!all(
    c("cohort_set", "cohort_count", "cohort_attrition") %in%
    names(attributes(cohortTable))
  )) {
    displayErrorMessage(
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
      displayErrorMessage(paste0(
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
    displayErrorMessage(c(
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
    displayErrorMessage(c(
      "Present cohort_definition_id must be the same in all elements",
      paste0("** cohort: ", cdiCohort),
      paste0("** cohort_set: ", cdiCohortSet),
      paste0("** cohort_attrition: ", cdiCohortCount),
      paste0("** cohort_count: ", cdiCohortAttrition)
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
  x <- class(x)
  x <- x[!grepl(pattern = "cohort", x = x, ignore.case = TRUE)]
  paste0(x, collapse = ", ")
}
cdi <- function(x) {
  x %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort() %>%
    paste0(collapse = ", ")
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

#' Get cohort settings from a cdm_cohort object.
#'
#' @param cohort A cdm_cohort object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet <- function(cohort) { UseMethod("cohortSet") }

#' @export
cohortSet.cdm_cohort <- function(cohort) {
  attr(cohort, "cohort_set") %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get cohort counts from a cdm_cohort object.
#'
#' @param cohort A cdm_cohort object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount <- function(cohort) { UseMethod("cohortCount") }

#' @export
cohortCount.cdm_cohort <- function(cohort) {
  attr(cohort, "cohort_attrition") %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) %>%
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get cohort attrition from a cdm_cohort object.
#'
#' @param cohort A cdm_cohort object.
#'
#' @return A table with the attrition.
#'
#' @export
cohortAttrition <- function(cohort) { UseMethod("cohortAttrition") }

#' @export
cohortAttrition.cdm_cohort <- function(cohort) {
  attr(cohort, "cohort_attrition") %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$cohort_definition_id)
}

