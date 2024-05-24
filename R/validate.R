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

#' Validate A cohortId input.
#'
#' @param cohortId A cohortId vector to be validated.
#' @param cohort A cohort_table object.
#'
#' @export
#'
validateCohortId <- function(cohortId, cohort) {
  assertNumeric(cohortId, integerish = TRUE, null = TRUE, min = 1, unique = TRUE)
  possibleCohortIds <- settings(cohort) |>
    dplyr::pull("cohort_definition_id") |>
    as.integer()
  if (is.null(cohortId)) {
    cohortId <- possibleCohortIds
  } else {
    cohortId <- as.integer(cohortId)
    notPresent <- cohortId[!cohortId %in% possibleCohortIds]
    if (length(notPresent) > 0) {
      cli::cli_abort("cohort definition id: {notPresent} not defined in settings.")
    }
  }
  return(cohortId)
}
