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

#' Bind two or more objects of the same class.
#'
#' @param ... Objects to bind.
#'
#' @return New object.
#'
#' @export
#'
bind <- function(...) {
  UseMethod("bind")
}

#' Bind two or more cohort tables
#'
#' @param ... Generated cohort set objects to bind. At least two must be
#' provided.
#' @param name Name of the new generated cohort set.
#'
#' @return The cdm object with a new generated cohort set containing all
#' of the cohorts passed.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cohort1 <- tibble(
#'   cohort_definition_id = 1,
#'   subject_id = 1:3,
#'   cohort_start_date = as.Date("2010-01-01"),
#'   cohort_end_date = as.Date("2010-01-05")
#' )
#' cohort2 <- tibble(
#'   cohort_definition_id = c(2, 2, 3, 3, 3),
#'   subject_id = c(1, 2, 3, 1, 2),
#'   cohort_start_date = as.Date("2010-01-01"),
#'   cohort_end_date = as.Date("2010-01-05")
#' )
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2025-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock",
#'   cohortTables = list("cohort1" = cohort1, "cohort2" = cohort2)
#' )
#'
#' cdm <- bind(cdm$cohort1, cdm$cohort2, name = "cohort3")
#' settings(cdm$cohort3)
#' cdm$cohort3
#'
bind.cohort_table <- function(..., name) {
  # initial checks
  cohorts <- list(...)
  assertList(cohorts, class = "cohort_table")
  assertCharacter(name, length = 1)

  # get cdm
  cdm <- cdmReference(cohorts[[1]])

  # bind
  newCohortSet <- lapply(cohorts, settings) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::mutate("new_cohort_definition_id" = dplyr::row_number())
  repeatedCohortName <- newCohortSet |>
    dplyr::group_by(.data$cohort_name) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::pull("cohort_name") |>
    unique()
  if (length(repeatedCohortName) > 0) {
    repeatedCohort <- lapply(repeatedCohortName, function(x) {
      newCohortSet |>
        dplyr::filter(.data$cohort_name == .env$x) |>
        dplyr::pull("cohort_id") |>
        paste0(collapse = ", ")
    }) |>
      unlist()
    err <- paste0(repeatedCohortName, " in ", repeatedCohort)
    cli::cli_abort("Cohorts can have the same cohort_name: {paste0(err, collapse = '; ')}.")
  }
  newCohortAttrition <- lapply(cohorts, attrition) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::left_join(
      newCohortSet |>
        dplyr::select(
          "cohort_definition_id", "cohort_id", "new_cohort_definition_id"
        ),
      by = c("cohort_definition_id", "cohort_id")
    ) |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_attrition")))
  newCohortCodelist <- lapply(cohorts, function(x) {
    xx <- attr(x, "cohort_codelist")
    if (is.null(xx)) {
      xx <- dplyr::tibble(
        "cohort_definition_id" = integer(),
        "codelist_name" = character(),
        "concept_id" = integer(),
        "type" = character()
      )
    } else {
      xx <- xx |>
        dplyr::collect() |>
        dplyr::mutate(
          "cohort_definition_id" = as.integer(.data$cohort_definition_id),
          "codelist_name" = as.character(.data$codelist_name),
          "concept_id" = as.integer(.data$concept_id),
          "type" = as.character(.data$type)
        ) |>
        dplyr::select(dplyr::all_of(cohortColumns("cohort_codelist")))
    }
    return(xx)
  }) |>
    dplyr::bind_rows(.id = "cohort_id") |>
    dplyr::left_join(
      newCohortSet |>
        dplyr::select(
          "cohort_definition_id", "cohort_id", "new_cohort_definition_id"
        ),
      by = c("cohort_definition_id", "cohort_id")
    ) |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_codelist")))

  # insert cohortSet
  nm <- uniqueTableName(tmpPrefix())
  cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = newCohortSet)
  cohorts <- lapply(seq_len(length(cohorts)), function(x) {
    cohorts[[x]] |>
      dplyr::left_join(
        cdm[[nm]] |>
          dplyr::filter(as.numeric(.data$cohort_id) == .env$x) |>
          dplyr::mutate(
            "cohort_definition_id" = as.integer(.data$cohort_definition_id),
            "cohort_name" = as.character(.data$cohort_name)
          ) |>
          dplyr::select("cohort_definition_id", "new_cohort_definition_id"),
        by = c("cohort_definition_id")
      ) |>
      dplyr::select(-"cohort_definition_id") |>
      dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
  })
  newCohort <- unionCohorts(cohorts) |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort"))) |>
    dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)
  newCohortSet <- newCohortSet |>
    dplyr::select(-c("cohort_definition_id", "cohort_id")) |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_set")))

  dropTable(cdm = cdm, name = nm)

  # instantiate the new generated cohort set
  cdm[[name]] <- newCohortTable(
    table = newCohort,
    cohortSetRef = newCohortSet,
    cohortAttritionRef = newCohortAttrition,
    cohortCodelistRef = newCohortCodelist,
    .softValidation = TRUE
  )

  return(cdm)
}

unionCohorts <- function(cohorts) {
  cols <- lapply(cohorts, colnames)
  allColumns <- cols |> unlist() |> unique()
  commonId <- lapply(allColumns, function(x) {
    lapply(cols, function(xx) {
      x %in% xx
    }) |>
      unlist() |>
      all()
  }) |>
    unlist() |>
    which()
  common <- allColumns[commonId]
  extra <- allColumns[!allColumns %in% common]

  if (length(extra) > 0) {
    for (k in seq_along(cohorts)) {
      extracols <- extra[!extra %in% colnames(cohorts[[k]])]
      missingCols <- missingColumns(cols = cols, extra = extracols)
      for (i in unique(missingCols)) {
        cohorts[[k]] <- cohorts[[k]] |>
          dplyr::left_join(
            cohorts[[i]] |>
              utils::head(1) |>
              dplyr::select(dplyr::all_of(c(
                "cohort_definition_id", names(missingCols[missingCols == i])
              ))) |>
              dplyr::filter(is.na(.data$cohort_definition_id)),
            by = "cohort_definition_id"
          )
      }
    }
  }

  Reduce(dplyr::union_all, cohorts)
}

missingColumns <- function(cols, extra) {
  lapply(extra, function(x) {
    lapply(cols, function(xx) {
      x %in% xx
    }) |>
      unlist() |>
      which() |>
      min()
  }) |>
    rlang::set_names(extra) |>
    unlist()
}

#' Bind two or summarised_result objects
#'
#' @param ... summarised_result objects
#'
#' @return A summarised_result object the merged objects.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = tibble(
#'       person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
#'       race_concept_id = 0, ethnicity_concept_id = 0
#'     ),
#'     "observation_period" = tibble(
#'       observation_period_id = 1:3, person_id = 1:3,
#'       observation_period_start_date = as.Date("2000-01-01"),
#'       observation_period_end_date = as.Date("2025-12-31"),
#'       period_type_concept_id = 0
#'     )
#'   ),
#'   cdmName = "mock",
#'   cohortTables = list("cohort1" = tibble(
#'     cohort_definition_id = 1,
#'     subject_id = 1:3,
#'     cohort_start_date = as.Date("2010-01-01"),
#'     cohort_end_date = as.Date("2010-01-05")
#'   ))
#' )
#'
#' result1 <- summary(cdm)
#' result2 <- summary(cdm$cohort1)
#'
#' mergedResult <- bind(result1, result2)
#' mergedResult
#'
bind.summarised_result <- function(...) {
  # initial checks
  results <- list(...) |>
    vctrs::list_drop_empty()
  assertList(results, class = "summarised_result")

  settings <- lapply(results, settings) |>
    dplyr::bind_rows(.id = "list_id")

  results <- results |>
    dplyr::bind_rows(.id = "list_id")

  cols <- colnames(settings)[!colnames(settings) %in% c("list_id", "result_id")]
  dic <- settings |>
    dplyr::select(!dplyr::all_of(c("list_id", "result_id"))) |>
    dplyr::distinct() |>
    dplyr::mutate("new_result_id" = as.integer(dplyr::row_number())) |>
    dplyr::inner_join(settings, by = cols) |>
    dplyr::select(c("list_id", "result_id", "new_result_id"))

  settings <- settings |>
    dplyr::inner_join(dic, by = c("result_id", "list_id")) |>
    dplyr::select(-c("result_id", "list_id")) |>
    dplyr::rename("result_id" = "new_result_id") |>
    dplyr::distinct()

  results <- results |>
    dplyr::inner_join(dic, by = c("result_id", "list_id")) |>
    dplyr::select(-c("result_id", "list_id")) |>
    dplyr::rename("result_id" = "new_result_id") |>
    newSummarisedResult(settings = settings)

  return(results)
}

#' @export
bind.NULL <- function(...) {
  x <- list(...) |>
    vctrs::list_drop_empty()
  if (length(x) == 0) return(NULL)
  bind(x)
}

#' @export
bind.list <- function(...) {
  if (length(list(...)) > 1) {
    cli::cli_abort("{.fn bind.list} only support one argument (a list).")
  }
  do.call(bind, ...)
}
