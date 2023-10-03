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

#' `omop_cohort` objects constructor.
#'
#' @param cohortTable Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetTable Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionTable Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCountTable Table with at least: cohort_definition_id,
#' number_subjects, number_records.
#' @param cohortName Name of the omop_cohort object.
#'
#' @return A omop_cohort object
#'
#' @export
#'
newOmopCohort <- function(cohortTable,
                         cohortSetTable = NULL,
                         cohortAttritionTable = NULL,
                         cohortCountTable = NULL,
                         cohortName = "cohort") {
  UseMethod("newOmopCohort")
}

#' `omop_cohort` objects constructor.
#'
#' @param cohortTable Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetTable Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionTable Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCountTable Table with at least: cohort_definition_id,
#' number_subjects, number_records.
#' @param cohortName Name of the omop_cohort object.
#'
#' @return A omop_cohort object
#'
#' @export
#'
newOmopCohort.tbl <- function(cohortTable,
                             cohortSetTable = NULL,
                             cohortAttritionTable = NULL,
                             cohortCountTable = NULL,
                             cohortName = "cohort") {
  # initial input check
  checkInput(
    cohortTable = cohortTable, cohortSetTable = cohortSetTable,
    cohortAttritionTable = cohortAttritionTable
  )

  if (!missing(cohortCountTable)) {
    cli::cli_warn(
      "cohortCountTable is no longer a required input to create an omop_cohort
      object"
    )
  }

  if (is.null(cohortSetTable)) {
    cohortSetTable <- createCohortSet(cohortTable)
  }
  attr(cohortTable, "cohort_set") <- cohortSetTable

  if (is.null(cohortAttritionTable)) {
    cohortAttritionTable <- createCohortAttrition(cohortTable)
  }
  attr(cohortTable, "cohort_attrition") <- cohortAttritionTable

  newClass <- c("omop_cohort",
   "GeneratedCohortSet" # to be removed
  )
  class(cohortTable) <- c(
    newClass, class(cohortTable)[!(class(cohortTable) %in% newClass)]
  )

  cohort <- validateCdmCohort(cohortTable)

  return(cohort)
}

#' To assess if an object is a valid `omop_cohort`
#'
#' @param cohortTable `omop_cohort` to validate.
#'
#' @export
#'
validateCdmCohort <- function(cohortTable) {
  UseMethod("validateCdmCohort")
}

#' To assess if an object is a valid `omop_cohort`.
#'
#' @param cohortTable Table to validate
#'
#' @export
#'
validateCdmCohort.omop_cohort <- function(cohortTable) {
  # attributes exist
  if (!all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cohortTable))
  )) {
    cli::cli_abort(
      "`cohort_set` and `cohort_attrition` must be attributes of a omop_cohort
      object."
    )
  }

  # get attributes
  cohort <- cohortTable
  cohort_set <- attr(cohortTable, "cohort_set")
  cohort_attrition <- attr(cohortTable, "cohort_attrition")

  # assert columns
  checkColumns <- function(x, cols, nam) {
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of ",
        nam, " of a omop_cohort object."
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
    attr(cohortTable, "cohort_attrition"),
    c("cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"),
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

#' To collect a `omop_cohort` object.
#'
#' @param x `omop_cohort` object.
#' @param ... Not used (for compatibility).
#'
#' @export
collect.omop_cohort <- function(x, ...) {
  attrib <- attributes(x)
  class(x) <- class(x)[class(x) != "omop_cohort"]
  x <- x %>% dplyr::collect()
  attr(x, "cohort_set") <- dplyr::collect(attrib$cohort_set)
  attr(x, "cohort_attrition") <- dplyr::collect(attrib$cohort_attrition)
  class(x) <- c("omop_cohort", class(x))
  return(x)
}

#' Get cohort settings from a omop_cohort object.
#'
#' @param cohort A omop_cohort object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet <- function(cohort) { UseMethod("cohortSet") }

#' @export
cohortSet.omop_cohort <- function(cohort) {
  attr(cohort, "cohort_set") %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get cohort counts from a omop_cohort object.
#'
#' @param cohort A omop_cohort object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount <- function(cohort) { UseMethod("cohortCount") }

#' @export
cohortCount.omop_cohort <- function(cohort) {
  attr(cohort, "cohort_attrition") %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) %>%
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) %>%
    dplyr::collect() %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$cohort_definition_id)
}

#' Get cohort attrition from a omop_cohort object.
#'
#' @param cohort A omop_cohort object.
#'
#' @return A table with the attrition.
#'
#' @export
cohortAttrition <- function(cohort) { UseMethod("cohortAttrition") }

#' @export
cohortAttrition.omop_cohort <- function(cohort) {
  attr(cohort, "cohort_attrition") %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$cohort_definition_id)
}
