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

#' Bind multiple cdm_reference objects.
#'
#' @param ... Multiple cdm_reference objects.
#' @param newCdmName Name of the new cdm object.
#'
#' @return New cdm_reference.
#'
#' @export
#'
bind.cdm_reference <- function(..., newCdmName = "BINDED CDM") {
  # initial checks
  cdmList = list(...)
  checkInput(cdmList = cdmList, newCdmName = newCdmName)

  cdm <- cdmList[[1]]
  for (k in 2:length(cdmList)) {
    cdm <- appendCdm(cdm, cdmList[[k]])
  }

  attr(cdm, "cdm_name") <- newCdmName

  return(cdm)
}

appendCdm <- function(cdm1, cdm2) {
  for (nam in names(cdm2)) {
    if (nam %in% names(cdm1)) {
      cdm1[[nam]] <- dplyr::union_all(cdm1[[nam]], cdm2[[nam]])
    } else {
      cdm1[[nam]] <- cdm2[[nam]]
    }
  }
  return(cdm1)
}

#' Bind multiple generated_cohort_set objects.
#'
#' @param ... Multiple generated_cohort_set objects.
#'
#' @return New generated_cohort_set
#'
#' @export
#'
bind.generated_cohort_set <- function(...) {
  # initial checks
  listOfCohorts = list(...)
  checkInput(listOfCohorts = listOfCohorts)

  # get ids
  ids <- getNewIds(listOfCohorts)

  # update ids
  listOfCohorts <- updateIds(listOfCohorts, ids)

  # bind cohort
  newCohort <- bindCohorts(listOfCohorts)

  return(newCohort)
}

getNewIds <- function(listOfCohorts) {
  ids <- dplyr::tibble(
    cohort_definition_id = numeric(), cohort_name = character(),
    cohort = numeric()
  )
  for (k in seq_along(listOfCohorts)) {
    cs <- cohortSet(listOfCohorts[[k]])
    ids <- ids |>
      dplyr::union_all(
        ids |>
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
          dplyr::mutate(
            new_cohort_definition_id = dplyr::row_number(), cohort = k
          ) |>
          dplyr::mutate(
            new_cohort_definition_id = .data$new_cohort_definition_id + max(
              ids$new_cohort_definition_id
            )
          )
      )
  }
  ids <- ids |> dplyr::select(-"cohort_name")
  return(ids)
}
updateIds <- function(listOfCohorts, ids) {
  for (k in seq_along(listOfCohorts)) {
    id <- ids |> dplyr::filter(.data$cohort == .env$k)
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
    dplyr::inner_join(id, by = "cohort_definition_id") |>
    dplyr::select(-"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::relocate("cohort_definition_id")
}
bindCohorts <- function(listOfCohorts) {
  cohort <- dplyr::bind_rows(listOfCohorts)
  attr(cohort, "cohort_set") <- dplyr::bind_rows(lapply(
    listOfCohorts, attr, "cohort_set"
  ))
  attr(cohort, "cohort_attrition") <- dplyr::bind_rows(lapply(
    listOfCohorts, attr, "cohort_attrition"
  ))
  return(cohort)
}
