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

#' Bind generated_cohort_set
#'
#' @param ... Generated cohort set objects to bind. At least two must be
#' provided.
#' @param name Name of the new generated cohort set.
#'
#' @return New generated cohort set
#'
#' @export
#'
bind.generated_cohort_set <- function(..., name) {
  # initial checks
  cohorts <- list(...)
  assertList(cohorts, class = "generated_cohort_set")
  if (length(cohorts) < 2) {
    cli::cli_abort(
      "Only {length(cohorts)} cohort provided, at least 2 must be provided."
    )
  }
  if (is.null(name)) {
    name <- attr(cohorts[[1]], "tbl_name")
  }
  assertCharacter(name, length = 1)

  # bind
}
