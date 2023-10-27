# Copyright 2023 DARWIN EU (C)
#
# This file is part of OMOPUtilities
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

#' Bind multiple generated_cohort_set objects.
#'
#' @param ... Multiple generated_cohort_set objects.
#' @param cohortName Name of the new bound cohort.
#'
#' @return New generated_cohort_set
#'
#' @export
#'
bind.generated_cohort_set <- function(..., cohortName = "bound_cohort") {
  # initial checks
  listOfCohorts = list(...)
  checkInput(listOfCohorts = listOfCohorts, cohortName = cohortName)

  # get ids
  ids <- getNewIds(listOfCohorts)

  # update ids
  listOfCohorts <- updateIds(listOfCohorts, ids)

  # bind cohort
  newCohort <- bindCohorts(listOfCohorts, cohortName)

  return(newCohort)
}

getNewIds <- function(listOfCohorts) {
  ids <- dplyr::tibble(
    cohort_definition_id = numeric(), cohort_name = character(),
    cohort = numeric(), new_cohort_definition_id = numeric()
  )
  id <- 0
  for (k in seq_along(listOfCohorts)) {
    cs <- cohortSet(listOfCohorts[[k]])
    ids <- ids |>
      dplyr::union_all(
        ids |>
          dplyr::select("cohort_name", "new_cohort_definition_id") |>
          dplyr::inner_join(
            cs |>
              dplyr::select("cohort_definition_id", "cohort_name"),
            by = "cohort_name"
          ) |>
          dplyr::mutate(cohort = k)
      ) |>
      dplyr::union_all(
        cs |>
          dplyr::anti_join(ids, by = "cohort_name") |>
          dplyr::select("cohort_definition_id", "cohort_name") |>
          dplyr::arrange(.data$cohort_definition_id) |>
          dplyr::mutate(
            new_cohort_definition_id = dplyr::row_number(), cohort = k
          ) |>
          dplyr::mutate(
            new_cohort_definition_id = .data$new_cohort_definition_id + id
          )
      )
    if (nrow(ids) > 0) {
      id <- max(ids$new_cohort_definition_id)
    } else {
      id <- 0
    }
  }
  ids <- ids |> dplyr::select(-"cohort_name")
  return(ids)
}
updateIds <- function(listOfCohorts, ids) {
  for (k in seq_along(listOfCohorts)) {
    id <- ids |>
      dplyr::filter(.data$cohort == .env$k) |>
      dplyr::select(-"cohort")
    listOfCohorts[[k]] <- correctId(listOfCohorts[[k]], id)
    attr(listOfCohorts[[k]], "cohort_set") <- correctId(
      attr(listOfCohorts[[k]], "cohort_set"), id
    )
    attr(listOfCohorts[[k]], "cohort_attrition") <- correctId(
      attr(listOfCohorts[[k]], "cohort_attrition"), id
    )
  }
  return(listOfCohorts)
}
correctId <- function(tab, id) {
  tab |>
    dplyr::inner_join(id, by = "cohort_definition_id", copy = TRUE) |>
    dplyr::select(-"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate("cohort_definition_id")
}
bindCohorts <- function(listOfCohorts, cohortName) {
  cohort <- dplyr::bind_rows(listOfCohorts)
  attr(cohort, "cohort_set") <- dplyr::bind_rows(lapply(
    listOfCohorts, attr, "cohort_set"
  ))
  attr(cohort, "cohort_attrition") <- dplyr::bind_rows(lapply(
    listOfCohorts, attr, "cohort_attrition"
  ))
  attr(cohort, "tbl_name") <- cohortName
  return(cohort)
}
