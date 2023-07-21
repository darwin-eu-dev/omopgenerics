# Copyright 2023 DARWIN EU (C)
#
# This file is part of PatientProfiles
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

#' CdmReference objects constructor.
#'
#' @param cohortRef Table with at least: cohort_definition_id, subject_id,
#' cohort_start_date, cohort_end_date.
#' @param cohortSetRef Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionRef Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCountRef Table with at least: cohort_definition_id,
#' number_subjects, number_records
#' @param cdm A cdm reference.
#'
#' @return A cdm reference
#'
#' @export
#'
newGeneratedCohortSet <- function(cohortRef,
                                  cohortSetRef,
                                  cohortAttritionRef,
                                  cohortCountRef,
                                  cdm) {
  # initial input check
  checkInput(
    cohortRef = cohortRef, cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef, cohortCountRef = cohortCountRef,
    cdm = cdm
  )

  attr(cohortRef, "cdm_reference") <- cdm
  attr(cohortRef, "cohort_set") <- cohortSetRef
  attr(cohortRef, "cohort_attrition") <- cohortCountRef
  attr(cohortRef, "cohort_count") <- cohortCountRef
  class(cohortRef) <- c(
    "GeneratedCohortSet",
    class(cohortRef)[class(cohortRef) != "GeneratedCohortSet"]
  )
  return(cohortRef)
}
