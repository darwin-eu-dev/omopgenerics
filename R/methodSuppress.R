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

#' Function to suppress counts in result objects
#'
#' @param result Result object
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table with suppressed counts
#'
#' @export
#'
suppress <- function(result,
                     minCellCount = 5) {
  UseMethod("suppress")
}

#' Function to suppress counts in result objects
#'
#' @param result summarised_result object.
#' @param minCellCount Minimum count of records to report results.
#'
#' @return summarised_result with suppressed counts.
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(omopgenerics)
#'
#' my_result <- tibble(
#'   "result_id" = "1",
#'   "cdm_name" = "mock",
#'   "result_type" = "summarised_characteristics",
#'   "package_name" = "omopgenerics",
#'   "package_version" = as.character(utils::packageVersion("omopgenerics")),
#'   "group_name" = "overall",
#'   "group_level" = "overall",
#'   "strata_name" = c(rep("overall", 6), rep("sex", 3)),
#'   "strata_level" = c(rep("overall", 6), "male", "female", "female"),
#'   "variable_name" = c("number records", "age_group", "age_group",
#'   "age_group", "age_group", "my_variable", "number records", "age_group",
#'   "age_group"),
#'   "variable_level" = c(NA, "<50", "<50", ">=50", ">=50", NA, NA,
#'   "<50", "<50"),
#'   "estimate_name" = c("count", "count", "percentage", "count", "percentage",
#'   "random", "count", "count", "percentage"),
#'   "estimate_type" = c("integer", "integer", "percentage", "integer",
#'   "percentage", "numeric", "integer", "integer", "percentage"),
#'   "estimate_value" = c("10", "5", "50", "3", "30", "1", "3", "12", "6"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' )
#' my_result <- newSummarisedResult(my_result)
#' my_result |> glimpse()
#' my_result <- suppress(my_result, minCellCount = 5)
#' my_result |> glimpse()
#'
suppress.summarised_result <- function(result,
                                       minCellCount = 5) {
  # initial checks
  assertClass(
    result, class = c("tbl", "data.frame", "summarised_result"), all = TRUE
  )
  assertNumeric(minCellCount, integerish = TRUE, min = 0, length = 1)

  # check if already suppressed
  set <- settings(result)
  if ("min_cell_count" %in% colnames(set)) {
    prevSupp <- unique(set |> dplyr::pull("min_cell_count")) |> as.numeric()
    if (prevSupp > minCellCount) {
      cli::cli_warn("Results passed are already obscured for counts smaller than {prevSupp}.")
      return(result)
    }
  }

  # suppression at cdm_name, group, strata and additional level
  groupSuppress <- c("number subjects", "number records")
  # suppression at cdm_name, group, strata, additional and variable level
  variableSuppress <- c(
    "count", "denominator_count", "outcome_count", "record_count",
    "subject_count"
  )
  # linked suppression
  linkedSuppression <- c(count = "percentage")
  # value of suppression
  suppressed <- NA_character_

  result <- result |>
    # suppress records
    suppressCounts(minCellCount) |>
    # suppress records by group
    suppressGroup(groupSuppress) |>
    # suppress records by variable
    suppressVariable(variableSuppress) |>
    # suppress records by linkage
    suppressLinkage(linkedSuppression) |>
    # suppress column
    suppressColumn(suppressed)

  # update settings
  set <- set |> dplyr::mutate("min_cell_count" = as.integer(.env$minCellCount))
  result <- newSummarisedResult(x = result, settings = set)

  return(result)
}

suppressCounts <- function(result, minCellCount) {
  result$suppress_record <- F
  result$is_count <- F
  id <- which(grepl("count", result$estimate_name))
  result$is_count[id] <- T
  estimates <- as.numeric(result$estimate_value[id])
  result$suppress_record[id[estimates > 0 & estimates < minCellCount]] <- T
  return(result)
}
suppressGroup <- function(result, groupSuppress) {
  obsLabels <- unique(result$variable_name)
  obsLabels <- obsLabels[tolower(stringr::str_replace_all(string = obsLabels,
                                                          pattern = "_",
                                                          replacement = " ")) %in% groupSuppress]

  supByGroup <- result |>
    dplyr::filter(
      .data$suppress_record & .data$variable_name %in% .env$obsLabels
    ) |>
    dplyr::select(!dplyr::starts_with(c(
      "variable", "estimate", "suppress", "is_count"
    ))) |>
    dplyr::mutate("suppress_group" = T) |>
    dplyr::distinct()
  joinCols <- colnames(supByGroup)[colnames(supByGroup) != "suppress_group"]
  result <- result |> dplyr::left_join(supByGroup, by = joinCols)
  result$suppress_group[is.na(result$suppress_group)] <- F
  return(result)
}
suppressVariable <- function(result, variableSuppress) {
  supByVariable <- result |>
    dplyr::filter(
      .data$suppress_record & .data$estimate_name %in% .env$variableSuppress
    ) |>
    dplyr::select(!dplyr::starts_with(c("estimate", "suppress", "is_count"))) |>
    dplyr::mutate("suppress_variable" = T) |>
    dplyr::distinct()
  joinCols <- colnames(supByVariable)[colnames(supByVariable) != "suppress_variable"]
  result <- result |> dplyr::left_join(supByVariable, by = joinCols)
  result$suppress_variable[is.na(result$suppress_variable)] <- F
  return(result)
}
suppressLinkage <- function(result, linkedSuppression) {
  supByLinkage <- list()
  for (k in seq_along(linkedSuppression)) {
    nm <- names(linkedSuppression)[k]
    subs <- linkedSuppression[k] |> unname()
    supByLinkage <- result |>
      dplyr::filter(
        .data$suppress_record & grepl(.env$nm, .data$estimate_name)
      ) |>
      dplyr::select(!dplyr::starts_with(c(
        "estimate_type", "estimate_value", "suppress", "is_count"
      ))) |>
      dplyr::mutate(
        "estimate_name" = gsub(.env$nm, .env$subs, .data$estimate_name)
      ) |>
      dplyr::mutate("suppress_linked" = T)
  }
  supByLinkage <- dplyr::bind_rows(supByLinkage) |> dplyr::distinct()

  joinCols <- colnames(supByLinkage)[colnames(supByLinkage) != "suppress_linked"]
  result <- result |> dplyr::left_join(supByLinkage, by = joinCols)
  result$suppress_linked[is.na(result$suppress_linked)] <- F
  return(result)
}
suppressColumn <- function(result, suppressed) {
  result |>
    dplyr::mutate("estimate_value" = dplyr::case_when(
      !.data$suppress_record & .data$is_count ~ .data$estimate_value,
      .data$suppress_record | .data$suppress_group | .data$suppress_variable |
        .data$suppress_linked ~ .env$suppressed,
      TRUE ~ .data$estimate_value
    )) |>
    dplyr::select(!dplyr::starts_with(c("suppress", "is_count")))
}
