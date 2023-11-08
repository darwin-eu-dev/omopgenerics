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
#' @param cohortRef Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetRef Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionRef Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortName Name of the generated_cohort_set object.
#' @param ... Other parameters.
#'
#' @return A generated_cohort_set object
#'
#' @export
#'
generatedCohortSet <- function(cohortRef,
                               cohortSetRef = NULL,
                               cohortAttritionRef = NULL,
                               cohortName = "cohort",
                               ...) {
  UseMethod("generatedCohortSet", cohortRef)
}

#' `generated_cohort_set` objects constructor.
#'
#' @param cohortRef Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetRef Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionRef Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortName Name of the generated_cohort_set object.
#' @param ... For compatibility.
#'
#' @return A generated_cohort_set object
#'
#' @export
#'
generatedCohortSet.tbl <- function(cohortRef,
                                   cohortSetRef = NULL,
                                   cohortAttritionRef = NULL,
                                   cohortName = "cohort",
                                   ...) {
  # initial checks
  rlang::check_dots_empty()
  assertClass(cohortRef, "tbl")
  assertClass(cohortSetRef, "tbl", null = TRUE)
  assertClass(cohortAttritionRef, "tbl", null = TRUE)
  assertCharacter(cohortName, length = 1, minNumCharacter = 1)

  # populate
  if (is.null(cohortSetRef)) {
    cohortSetRef <- defaultCohortSet(cohortRef)
  }
  if (is.null(cohortAttritionRef)) {
    cohortAttritionRef <- defaultCohortAttrition(cohortRef, cohortSetRef)
  }

  # constructor
  cohort <- constructGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortName = cohortName
  )

  # validate
  cohort <- validateGeneratedCohortSet(cohort)

  # return
  return(cohort)
}

#' To collect a `generated_cohort_set` object.
#'
#' @param x `generated_cohort_set` object.
#' @param ... Not used (for compatibility).
#'
#' @return A data frame with yhe `generated_cohort_set`
#'
#' @export
#'
#' @importFrom dplyr collect
#'
collect.generated_cohort_set <- function(x, ...) {
  attrib <- attributes(x)
  class(x) <- class(x)[class(x) != "generated_cohort_set"]
  x <- x |> dplyr::collect()
  attr(x, "cohort_set") <- dplyr::collect(attrib$cohort_set)
  attr(x, "cohort_attrition") <- dplyr::collect(attrib$cohort_attrition)
  class(x) <- c("generated_cohort_set", class(x))
  return(x)
}

#' Get cohort settings from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet <- function(cohort) {
  UseMethod("cohortSet", cohort)
}

#' Get cohort settings from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet.generated_cohort_set <- function(cohort) {
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
  UseMethod("cohortCount", cohort)
}

#' Get cohort counts from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount.generated_cohort_set <- function(cohort) {
  attr(cohort, "cohort_attrition") |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) |>
    dplyr::collect() |>
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
  UseMethod("cohortAttrition", )
}

#' Get cohort attrition from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the attrition.
#'
#' @export
cohortAttrition.generated_cohort_set <- function(cohort) {
  attr(cohort, "cohort_attrition") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id)
}

constructGeneratedCohortSet <- function(cohortRef,
                                        cohortSetRef,
                                        cohortAttritionRef,
                                        cohortName) {
  attr(cohortRef, "cohort_set") <- cohortSetRef
  attr(cohortRef, "cohort_attrition") <- cohortAttritionRef
  newClass <- c("generated_cohort_set", "GeneratedCohortSet")
  class(cohortRef) <- c(
    newClass, class(cohortRef)[!(class(cohortRef) %in% newClass)]
  )
  attr(cohortRef, "tbl_name") <- cohortName

  return(cohortRef)
}
validateGeneratedCohortSet <- function(cohort) {
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
  checkColumnsCohort <- function(x, cols, nam) {
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of ",
        nam, " of a generated_cohort_set object."
      ))
    }
    invisible(NULL)
  }
  checkColumnsCohort(
    cohort,
    c(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ),
    "the cohort"
  )
  checkColumnsCohort(
    cohort_set, c("cohort_definition_id", "cohort_name"), "the cohort_set"
  )
  checkColumnsCohort(
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
      "*" = "class(cohort) = {clCohort}",
      "*" = "class(cohort_set) = {clCohortSet}",
      "*" = "class(cohort_attrition) = {clCohortAttrition}"
    ))
  }

  # cohort_definition_id the same
  cdiCohort <- cdi(cohort)
  cdiCohortSet <- cdi(cohort_set)
  cdiCohortAttrition <- cdi(cohort_attrition)
  if (!equal(cdiCohort, cdiCohortSet, cdiCohortAttrition)) {
    cli::cli_abort(c(
      "Present cohort_definition_id must be the same in all elements",
      "*" = "cohort: {cdiCohort}",
      "*" = "cohort_set: {cdiCohortSet}",
      "*" = "cohort_attrition: {cdiCohortAttrition}"
    ))
  }

  # cohort_name column
  cohortNames <- cohort_set |> dplyr::pull("cohort_name")
  if (length(cohortNames) != length(unique(cohortNames))) {
    cli::cli_abort("cohort_name in the cohort_set must be unique")
  }
  notSnake <- cohortNames[!isSnakeCase(cohortNames)]
  if (length(notSnake)) {
    cli::cli_abort(
      "cohort_name must be snake case: {paste0(notSnake, collapse = ', ')}"
    )
  }

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
defaultCohortSet <- function(cohort) {
  cohort |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::mutate("cohort_name" = paste0("cohort_", .data$cohort_definition_id))
}
defaultCohortAttrition <- function(cohort, set = NULL) {
  cohort <- cohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    )
  if (!is.null(set)) {
    cohort <- cohort |>
      dplyr::left_join(
        set |> dplyr::select("cohort_definition_id"),
        by = "cohort_definition_id"
      )
  }
  cohort |>
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
