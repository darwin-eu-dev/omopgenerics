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

#' 'compared_results' object constructor
#'
#' @param x input must be a tibble
#' @param name Name of the compared result object
#'
#' @return A compared_result object
#' @export
#'
comparedResult <- function(x, name = "compared_result") {

  #inital input check
  assertTibble(x)
  assertCharacter(name, length = 1, minNumCharacter = 1)

  #constructer
  x <- newComparedResult(x, name)

  # validate
  x <- validateComparedResult(x)

  return(x)
}


newComparedResult <- function(x, name) {

  class(x) <- c("compared_result", class(x))
  attr(x, "compared_result_name") <- name

  return(x)
}


validateComparedResult <- function(x) {

  # class
  if (!"compared_result" %in% class(x)) {
    cli::cli_abort("The tibble does not has the summarised_result class")
  }

  # compulsory columns
  compulsoryCols <- c(
    "cdm_name", "result_type", "package", "package_version",
    "group_name_reference", "group_level_reference", "strata_name_reference",
    "strata_level_reference", "group_name_comparator", "group_level_comparator",
    "strata_name_comparator", "strata_level_comparator", "variable",
    "variable_level", "variable_type", "estimate_type", "estimate"
  )
  checkColumns(x = x, cols = compulsoryCols)

  # all columns should be character
  checkColumnsFormat(x = x, cols = compulsoryCols, format = "character")

  # Cannot contain NA columns
  notNaCols <- c(
    "cdm_name", "group_name_reference", "group_level_reference",
    "strata_name_reference", "strata_level_reference", "group_name_comparator",
    "group_level_comparator", "strata_name_comparator",
    "strata_level_comparator", "variable", "variable_type", "estimate_type",
    "estimate"
  )
  checkNA(x = x, cols = notNaCols)

  #Sentence case column
  sentenceCaseCols <- c("result_type")
  checkSentence(x = x, cols = sentenceCaseCols)

  # columPairs
  columnPairs <- c(
    "group_name_reference" = "group_level_reference",
    "strata_name_reference" = "strata_level_reference",
    "group_name_comparator" = "group_level_comparator",
    "strata_name_comparator" = "strata_level_comparator"
  )
  checkColumnPairs(x, columnPairs, " and ", "NA")

  return(x)
}
