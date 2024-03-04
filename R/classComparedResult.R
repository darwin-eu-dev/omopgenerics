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

#' 'compared_results' object constructor
#'
#' @param x input must be a tibble
#'
#' @return A compared_result object
#' @export
#'
newComparedResult <- function(x) {

  lifecycle::deprecate_warn(
    when = "0.1.0", what = "newComparedResult()", with = "newSummarisedResult()"
  )

  #inital input check
  assertClass(x, "data.frame")

  #constructer
  x <- constructComparedResult(x)

  # validate
  x <- validateComparedResult(x)

  return(x)
}

constructComparedResult <- function(x) {
  x |>
    addClass("compared_result")
}
validateComparedResult <- function(x) {
  if (!"result_id" %in% colnames(x)) {
    x <- x |> dplyr::mutate("result_id" = NA_character_)
    cli::cli_alert_warning(
      "`result_id` column is missing, please add it as it is a compulsory column."
    )
  }

  # compulsory columns
  x <- checkColumns(x = x, "compared_result")

  # all columns should be character
  x <- checkColumnsFormat(x = x, "compared_result")

  # Cannot contain NA columns
  checkNA(x = x, "compared_result")

  # columPairs
  columnPairs <- c(
    "group_name_reference" = "group_level_reference",
    "strata_name_reference" = "strata_level_reference",
    "group_name_comparator" = "group_level_comparator",
    "strata_name_comparator" = "strata_level_comparator",
    "additional_name_reference" = "additional_level_reference",
    "additional_name_comparator" = "additional_level_comparator"
  )
  checkColumnPairs(x, columnPairs, " and | &&& ", "snake")

  # estimate_type
  checkColumnContent(
    x = x, col = "estimate_type", content = estimateTypeChoices()
  )

  return(x)
}

#' Empty `compared_result` object.
#'
#' @return An empty `compared_result` object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#'
#' emptyComparedResult()
#' }
#'
emptyComparedResult <- function() {
  lifecycle::deprecate_warn(
    when = "0.1.0",
    what = "emptyComparedResult()",
    with = "emptySummarisedResult()"
  )
  resultColumns("compared_result") |>
    rlang::rep_named(list(character())) |>
    dplyr::as_tibble() |>
    newComparedResult()
}
