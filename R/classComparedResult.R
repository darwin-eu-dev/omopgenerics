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
#'
#' @return A compared_result object
#' @export
#'
comparedResult <- function(x) {

  #inital input check
  assertClass(x, "data.frame")

  #constructer
  x <- newComparedResult(x)

  # validate
  x <- validateComparedResult(x)

  return(x)
}

newComparedResult <- function(x) {
  x <- getClass(x, "compared_result")
  return(x)
}

validateComparedResult <- function(x) {
  # compulsory columns
  compulsoryCols <- c(
    "cdm_name",
    "result_type",
    "package_name", "package_version",
    "group_name_reference", "group_level_reference",
    "strata_name_reference", "strata_level_reference",
    "group_name_comparator", "group_level_comparator",
    "strata_name_comparator", "strata_level_comparator",
    "variable_name", "variable_level", "variable_type",
    "estimate_name", "estimate_type", "estimate_value",
    "additional_name", "additional_level"
  )
  x <- checkColumns(x = x, cols = compulsoryCols, "compared_result")

  # all columns should be character
  checkColumnsFormat(x = x, cols = compulsoryCols, format = "character")

  # Cannot contain NA columns
  notNaCols <- c(
    "cdm_name", "group_name_reference", "group_level_reference",
    "strata_name_reference", "strata_level_reference",
    "group_name_comparator", "group_level_comparator",
    "strata_name_comparator", "strata_level_comparator",
    "variable_name", "variable_type", "estimate_name", "estimate_type",
    "additional_name", "additional_level"
  )
  checkNA(x = x, cols = notNaCols)

  #Sentence case column
  sentenceCaseCols <- c("variable_name", "variable_level")
  checkSentence(x = x, cols = sentenceCaseCols)

  # columPairs
  columnPairs <- c(
    "group_name_reference" = "group_level_reference",
    "strata_name_reference" = "strata_level_reference",
    "group_name_comparator" = "group_level_comparator",
    "strata_name_comparator" = "strata_level_comparator",
    "additonal_name" = "additional_level"
  )
  checkColumnPairs(x, columnPairs, " and ", "snake")

  # estimate_type
  checkColumnContent(x, "estimate_type", c("numeric", "date", "character"))

  return(x)
}
