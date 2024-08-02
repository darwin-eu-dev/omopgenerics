# Copyright 2024 DARWIN EU (C)
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

#' Validate name argument.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A cdm_reference object. It will check if a table named name
#' already exists in the cdm.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateNameArgument <- function(name,
                                 cdm = NULL,
                                 validation = "error",
                                 call = parent.frame()) {
  assertValidation(validation)
  nm <- substitute(name) |> utils::capture.output()
  assertCharacter(name, length = 1, call = call)
  newName <- toSnakeCase(name)
  if (newName != name) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "`{nm}` is not snake_case it should be modified to: {newName}"))
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "`{nm}` was modified: {name} -> {newName}"))
    }
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      if (validation == "error") {
        cli::cli_abort(c("!" = "There already exist a table named {.var {newName}}."))
      } else if (validation == "warning") {
        cli::cli_warn(c("!" = "There already exist a table named {.var {newName}}. It will be overwritten."))
      }
    }
  }
  return(newName)
}

#' Validate a cohort table input.
#'
#' @param cohort Object to be validated as a valid cohort input.
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on
#' or after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have
#' overlapping cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateCohortArgument <- function(cohort,
                                   checkEndAfterStart = TRUE,
                                   checkOverlappingEntries = TRUE,
                                   checkMissingValues = TRUE,
                                   checkInObservation = TRUE,
                                   validation = "error",
                                   call = parent.frame()) {
  assertValidation(validation)
  assertLogical(checkEndAfterStart, length = 1)
  assertLogical(checkOverlappingEntries, length = 1)
  assertLogical(checkMissingValues, length = 1)
  assertLogical(checkInObservation, length = 1)

  assertClass(cohort, class = c("cohort_table", "cdm_table"), all = TRUE, call = call)

  if(is.na(tableName(cohort))){
    missingCohortTableNameError(cdm, validation = validation)
    }

  # columns
  notPresent <- cohortColumns("cohort")[!cohortColumns("cohort") %in% colnames(cohort)]
  if (length(notPresent) > 0) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    }
  }
  cohort <- cohort |> dplyr::relocate(dplyr::any_of(cohortColumns("cohort")))

  if (isTRUE(checkEndAfterStart)) {
    cohort <- checkStartEnd(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkOverlappingEntries)) {
    cohort <- checkOverlap(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkMissingValues)) {
    cohort <- checkNaCohort(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkInObservation)) {
    cohort <- checkObservationPeriod(cohort = cohort, validation = validation, call = call)
  }
  return(cohort)
}

#' Validate cohortId argument.
#'
#' @param cohortId A cohortId vector to be validated.
#' @param cohort A cohort_table object.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateCohortIdArgument <- function(cohortId,
                                     cohort,
                                     validation = "error",
                                     call = parent.frame()) {
  assertValidation(validation)
  assertNumeric(cohortId, integerish = TRUE, null = TRUE, min = 1, unique = TRUE, call = call)
  assertClass(cohort, class = "cohort_table", call = call)
  possibleCohortIds <- settings(cohort) |>
    dplyr::pull("cohort_definition_id") |>
    as.integer()
  if (is.null(cohortId)) {
    cohortId <- possibleCohortIds
  } else {
    cohortId <- as.integer(cohortId)
    notPresent <- cohortId[!cohortId %in% possibleCohortIds]
    cohortId <- cohortId[cohortId %in% possibleCohortIds]
    if (length(notPresent) > 0) {
      if (validation == "error" | length(cohortId) == 0) {
        cli::cli_abort("cohort definition id: {notPresent} not defined in settings.", call = call)
      } else if (validation == "warning") {
        cli::cli_warn(c("!" = "cohort definition id: {notPresent} not considered as they are not defined in settings."), call = call)
      }
    }
  }
  return(cohortId)
}

#' Validate conceptSet argument.
#'
#' @param conceptSet It can be either a named list of concepts or a codelist,
#' codelist_with_details or conceptSetExpression object.
#' @param cdm A cdm_reference object, needed if a conceptSetExpression is
#' provided.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateConceptSetArgument <- function(conceptSet,
                                       cdm = NULL,
                                       validation = "error",
                                       call = parent.frame()) {
  if (inherits(conceptSet, "codelist")) {
    conceptSet <- validateCodelist(conceptSet, call = call)
  } else if (inherits(conceptSet, "codelist_with_details")) {
    conceptSet <- validateCodelistWithDetails(conceptSet, call) |>
      lapply(dplyr::pull, "concept_id")
  } else if (inherits(conceptSet, "conceptSetExpression")) {
    concepts <- validateConceptSetExpression(conceptSet, call)
    concepts <- concepts |>
      lapply(dplyr::select, c("concept_id", "excluded", "decendants")) |>
      dplyr::bind_rows(.id = "concept_name")
    descendants <- concepts |>
      dplyr::filter(.data$decendants == TRUE) |>
      dplyr::select(-"decendants")
    if (nrow(descendants) > 0) {
      assertClass(cdm, "cdm_reference")
      nm <- uniqueTableName()
      cdm <- insertTable(cdm = cdm, name = nm, table = descendants)
      descendants <- cdm$concept_ancestor |>
        dplyr::rename("to_join" = "ancestor_concept_id") |>
        dplyr::inner_join(
          cdm[[nm]] |>
            dplyr::rename("to_join" = "concept_id"),
          by = "concept_id"
        ) |>
        dplyr::select(
          "concept_id" = "descendant_concept_id", "excluded", "concept_name"
        ) |>
        dplyr::collect()
      concepts <- concepts |>
        dplyr::filter(.data$descendants == FALSE) |>
        dplyr::select(-"decendants") |>
        dplyr::union_all(descendants)
    } else {
      concepts <- concepts |> dplyr::select(-"decendants")
    }
    conceptSet <- concepts |>
      dplyr::filter(.data$excluded == FALSE) |>
      dplyr::select(-"excluded") |>
      dplyr::anti_join(
        concepts |>
          dplyr::filter(.data$excluded == TRUE),
        by = c("concept_id", "concept_name")
      ) |>
      dplyr::group_by(.data$concept_name) |>
      dplyr::group_split() |>
      as.list() |>
      lapply(function(x) {
        nm <- unique(x$concept_name)
        list(unique(x$concept_id)) |> rlang::set_names(nm)
      }) |>
      unlist(recursive = F)
  } else {
    conceptSet <- newCodelist(conceptSet)
  }
  return(conceptSet)
}

assertValidation <- function(validation, call = parent.frame()) {
  validation |>
    assertChoice(choices = c("error", "warning"), length = 1, call = call)
}



#' validateAgeGroupArgument
#'
#' @param ageGroup age group in a list
#' @param multipleAgeGroup allow mutliple age group
#' @param overlap allow overlapping ageGroup
#' @param call parent frame
#'
#' @return validate ageGroup
#' @export
#'
validateAgeGroupArgument <- function(ageGroup,
                                     multipleAgeGroup = TRUE,
                                     overlap = FALSE,
                                     call = parent.frame()){

  assertList(ageGroup, null = TRUE, call = call)
  assertLogical(multipleAgeGroup, length = 1)
  assertLogical(overlap, length = 1)

  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
  #check multiple age group

  if (!isTRUE(multipleAgeGroup)) {

    if (inherits(ageGroup[[2]],"list")){
      cli::cli_abort("Multiple age group are not allowed")
    }

  }

    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]], overlap))
      if (any(ageGroup[[k]] |> unlist() |> unique() < 0)) {
        cli::cli_abort("ageGroup can't contain negative values")
      }
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  invisible(ageGroup)

}

#' @noRd
checkCategory <-
  function(category,
           overlap = FALSE,
           type = "numeric",
           call = parent.frame()) {
    assertList(category, class = type, call = call)

    if (is.null(names(category))) {
      names(category) <- rep("", length(category))
    }

    # check length
    category <- lapply(category, function(x) {
      if (length(x) == 1) {
        x <- c(x, x)
      } else if (length(x) > 2) {
        cli::cli_abort("Please specify only two values
                       (lower bound and upper bound) per category",
                       call = call)
      }
      invisible(x)
    })

    # check lower bound is smaller than upper bound
    checkLower <- unlist(lapply(category, function(x) {
      x[1] <= x[2]
    }))
    if (!(all(checkLower))) {
      "Lower bound should be equal or smaller than upper bound" |>
        cli::cli_abort(call = call)
    }

    # built tibble
    result <- lapply(category, function(x) {
      dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(category_label = names(.env$category)) |>
      dplyr::mutate(
        category_label = dplyr::if_else(
          .data$category_label == "",
          dplyr::case_when(
            is.infinite(.data$lower_bound) &
              is.infinite(.data$upper_bound) ~ "any",
            is.infinite(.data$lower_bound) ~ paste(.data$upper_bound,
                                                   "or below"),
            is.infinite(.data$upper_bound) ~ paste(.data$lower_bound,
                                                   "or above"),
            TRUE ~ paste(.data$lower_bound, "to", .data$upper_bound)
          ),
          .data$category_label
        )
      ) |>
      dplyr::arrange(.data$lower_bound)

    # check overlap
    if (!overlap) {
      if (nrow(result) > 1) {
        lower <- result$lower_bound[2:nrow(result)]
        upper <- result$upper_bound[1:(nrow(result) - 1)]
        if (!all(lower > upper)) {
          "There can not be overlap between categories" |>
            cli::cli_abort(call = call)
        }
      }
    }

    invisible(result)
  }
#' validateCdmArgument
#'
#' @param cdm A cdm_reference object
#' @param checkOverlapObservation TRUE to perform check on no overlap observation period
#' @param checkStartBeforeEndObservation TRUE to perform check on correct observational start and end date
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @return A cdm_reference object
#' @export
#'
validateCdmArgument <- function(cdm,
                                checkOverlapObservation = FALSE,
                                checkStartBeforeEndObservation = FALSE,
                                validation = "error",
                                call = parent.frame()) {
  assertValidation(validation, call = parent.frame())
  assertLogical(checkOverlapObservation,
                length = 1,
                call = parent.frame())
  assertLogical(checkStartBeforeEndObservation,
                length = 1,
                call = parent.frame())


  # validate
    # assert class
    assertClass(cdm,
                class = c("cdm_reference"),
                all = TRUE,
                call = call)

  # not overlapping periods
  if (isTRUE(checkOverlapObservation)){
    checkOverlapObservation(cdm$observation_period)
  }

  # no start observation before end
  if (isTRUE(checkStartBeforeEndObservation)){
    checkStartBeforeEndObservation(cdm$observation_period)
  }

  return(invisible(cdm))



}


#' validateResultArguemnt
#'
#' @param result summarise result object to validate
#' @param validation message to return
#' @param call parent.frame
#'
#' @return summarise result object
#' @export
#'
validateResultArguemnt <- function(result,
                                   validation = "error",
                                   call = parent.frame()) {
  assertValidation(validation, call = parent.frame())
  assertTable(result, call = parent.frame())

  result <- validateSummariseResult(result)

  return(invisible(result))

  }


