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

#' Create a cdm object from local tables
#'
#' @param tables List of tables to be part of the cdm object.
#' @param cdmName Name of the cdm object.
#' @param cohortTables List of tables that contains cohort, cohort_set and
#' cohort_attrition can be provided as attributes.
#'
#' @return A `cdm_reference` object.
#'
#' @export
#'
cdmFromTables <- function(tables,
                          cdmName,
                          cohortTables = list()) {
  # check input
  assertCharacter(cdmName, length = 1)
  assertList(tables, named = TRUE, class = "data.frame")
  assertList(cohortTables, named = TRUE, class = "data.frame")

  src <- localSource()
  tables <- lapply(names(tables), function(nm) {
    tables[[nm]] |>
      dplyr::as_tibble() |>
      cdmTable(src = src, name = nm)
  })
  cohortTables <- lapply(names(cohortTables), function(nm) {
    cohortTables[[nm]] |>
      dplyr::as_tibble() |>
      cdmTable(src = src, name = nm) |>
      generatedCohortSet(
        cohortSetRef = attr(cohortTables[[nm]], "cohort_set"),
        cohortAttritionRef = attr(cohortTables[[nm]], "cohort_attrition"),
        overwrite = TRUE
      )
  })

  cdm <- cdmReference(tables = c(tables, cohortTables), cdmName = cdmName)

  return(cdm)
}

localSource <- function() {
  structure(.Data = list(), class = "local_cdm") |>
    cdmSource(sourceType = "local")
}

