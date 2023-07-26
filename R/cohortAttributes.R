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

#' Get cohort settings from a GeneratedCohortSet object.
#'
#' @param cohort A GeneratedCohortSet object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet <- function(cohort) { UseMethod("cohortSet") }

#' @export
cohortSet.GeneratedCohortSet <- function(cohort) {
  attr(cohort, "cohort_set")
}

#' Get cohort counts from a GeneratedCohortSet object.
#'
#' @param cohort A GeneratedCohortSet object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount <- function(cohort) { UseMethod("cohortCount") }

#' @export
cohortCount.GeneratedCohortSet <- function(cohort) {
  attr(cohort, "cohort_count")
}

#' Get cohort attrition from a GeneratedCohortSet object.
#'
#' @param cohort A GeneratedCohortSet object.
#'
#' @return A table with the attrition.
#'
#' @export
cohortAttrition <- function(cohort) { UseMethod("cohortAttrition") }

#' @export
cohortAttrition.GeneratedCohortSet <- function(cohort) {
  attr(cohort, "cohort_attrition")
}
