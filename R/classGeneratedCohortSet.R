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
#'
#' @return A generated_cohort_set object
#'
#' @export
#'
generatedCohortSet <- function(cohortRef,
                               cohortSetRef = NULL,
                               cohortAttritionRef = NULL) {
  # initial checks
  assertClass(cohortRef, "tbl")
  assertClass(cohortSetRef, "tbl", null = TRUE)
  assertClass(cohortAttritionRef, "tbl", null = TRUE)

  # comes from a cdm
  if (!"cdm_table" %in% class(cohortRef)) {
    cli::cli_abort(
      "The cohort does not come from a cdm object, please insert it with
      insertTable first"
    )
  }

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

constructGeneratedCohortSet <- function(cohortRef,
                                        cohortSetRef,
                                        cohortAttritionRef,
                                        cohortName) {
  attr(cohortRef, "cohort_set") <- cdmTable(cohortSetRef)
  attr(cohortRef, "cohort_attrition") <- cdmTable(cohortAttritionRef)
  addClass(cohortRef) <- c("generated_cohort_set", "GeneratedCohortSet")
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

  # get attributes
  cohort_set <- attr(cohort, "cohort_set")
  cohort_attrition <- attr(cohort, "cohort_attrition")

  # check name
  assertCharacter(attr(cohort, "tbl_name"), length = 1)
  assertCharacter(attr(cohort_set, "tbl_name"), length = 1)
  assertCharacter(attr(cohort_atrition, "tbl_name"), length = 1)

  # assert columns
  checkColumnsCohort <- function(x, nam) {
    cols <- requiredCohortColumns(nam)
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of the ",
        nam, " of a generated_cohort_set object."
      ))
    }
    invisible(NULL)
  }
  checkColumnsCohort(cohort, "cohort")
  checkColumnsCohort(cohort, "cohort_set")
  checkColumnsCohort(cohort, "cohort_attrition")

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

  # check NA
  checkNaCohort(cohort)

  # check within observation period
  checkObservationPeriod(cohort)

  # cehck overlap
  checkOverlap(cohort)

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
    sort() |>
    paste0(collapse = ", ")
}
defaultCohortSet <- function(cohort) {
  cohortName <- attr(cohort, "tbl_name")
  cohort |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::mutate(
      "cohort_name" = paste0("cohort_", as.character(.data$cohort_definition_id))
    ) |>
    dplyr::compute(name = paste0(cohortName, "_set"))
}
defaultCohortAttrition <- function(cohort, set) {
  cohortName <- attr(cohort, "tbl_name")
  cohort <- cohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    dplyr::left_join(
      set |> dplyr::select("cohort_definition_id"),
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
    ) |>
    dplyr::compute(name = paste0(cohortName, "_attrition"))
}
checkOverlap <- function(cohort) {
  x <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subjet_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(
      "next_cohort_start_date" = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::filter(.data$cohort_end_date >= .data$next_cohort_start_date) |>
    dplyr::collect()
  if (nrow(x) > 0){
    x5 <- x |>
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date", "next_cohort_start_date"
      ) |>
      utils::head(5)
    cli::cli_abort(
      "There is overlap between entries in the cohort, {nrow(x)} overlaps
      detected{ifelse(nrow(x)<=5), ':', ' first 5:')}",
      capture.output(print(x5, width = Inf))
    )
  }
  return(invisible(TRUE))
}
checkNaCohort <- function(cohort) {
  x <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ is.na(.x))) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x <- x |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(is.na(.data$value)) |>
      dplyr::pull("name") |>
      unique() |>
      paste0(collapse = ", ")
    cli::cli_abort(
      "Cohort can't have NA values, there are NA values in the following
      columns: {x}"
    )
  }
  return(invisible(TRUE))
}
checkObservationPeriod <- function(cohort) {
  cdm <- attr(cohort, "cdm_reference")
  if (is.null(cdm)) {
    cli::cli_abort("cdm_reference can not be NULL")
  }
  x <- cohort |>
    dplyr::anti_join(
      cohort |>
        dplyr::select(dplyr::all_of(requiredCohortColumns("cohort"))) |>
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
      by = requiredCohortColumns("cohort")
    )
  if (nrow(x) > 0) {
    x5 <- x |> utils::head(5)
    cli::cli_abort(
      "Some observations are not during observation period, {nrow(x)}
      detected{ifelse(nrow(x)<=5), ':', ' first 5:')}",
      capture.output(print(x5, width = Inf))
    )
  }
  return(invisible(TRUE))
}
