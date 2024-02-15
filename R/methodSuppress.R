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
#' @param suppressedValue Value to show in suppressed counts. If NULL suppressed
#' rows will be dropped.
#' @param suppressedGroup Value to show in suppressed group counts. If NULL
#' suppressed rows will be dropped.
#'
#' @return Table with suppressed counts
#' @export
#'
suppress <- function(result,
                     minCellCount = 5,
                     suppressedValue = paste0("<", minCellCount),
                     suppressedGroup = NA) {
  UseMethod("suppress")
}

#' @export
suppress.omop_result <- function(result,
                                       minCellCount = 5,
                                       suppressedValue = paste0("<", minCellCount),
                                       suppressedGroup = NA) {
  estimateName = "count"
  groupCount = c("number subjects", "number records")

  # initial checks
  assertTibble(result)
  assertNumeric(minCellCount, integerish = TRUE, min = 0, length = 1)
  suppressedValue <- ifelse(
    !is.null(suppressedValue), as.character(suppressedValue), suppressedValue
  )
  assertCharacter(suppressedValue, length = 1, null = TRUE, na = TRUE)
  suppressedGroup <- ifelse(
    !is.null(suppressedGroup), as.character(suppressedGroup), suppressedGroup
  )
  assertCharacter(suppressedGroup, length = 1, null = TRUE, na = TRUE)

  result <- result |>
    # obscured records
    obscureRecords(minCellCount, estimateName) |>
    # obscured records by group
    obscureGroup(minCellCount, estimateName, groupCount) |>
    # obscure column
    obscureColumn(suppressedValue, suppressedGroup)

  return(result)
}

obscureRecords <- function(result, minCellCount, estimateName) {
  recordsToObscure <- result |>
    dplyr::filter(.data$estimate_name == .env$estimateName) |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::filter(
      .data$estimate_value > 0 & .data$estimate_value < .env$minCellCount
    ) |>
    dplyr::mutate(
      "estimate_value" = as.character(.data$estimate_value),
      "obscure_record" = 1
    )
  result <- result |>
    dplyr::left_join(recordsToObscure, by = colnames(result)) |>
    dplyr::mutate(
      obscure_record = dplyr::if_else(is.na(.data$obscure_record), 0, 1)
    )
  return(result)
}
obscureGroup <- function(result, minCellCount, estimateName, groupCount) {
  groupsToObscure1 <- result |>
    dplyr::group_by(dplyr::across(!c(
      "estimate_name", "estimate_type", "estimate_value", "obscure_record"
    ))) |>
    dplyr::summarise(
      "obscure_group_1" = max(.data$obscure_record), .groups = "drop"
    )
  cols1 <- colnames(groupsToObscure1)[colnames(groupsToObscure1) != "obscure_group_1"]
  groupsToObscure2 <- result |>
    dplyr::filter(
      .data$variable_name %in% .env$groupCount &
        .data$estimate_name %in% .env$estimateName &
        .data$obscure_record == 1
    ) |>
    dplyr::select(!c(
      "variable_name", "variable_level", "estimate_name", "estimate_type",
      "estimate_value", "obscure_record"
    )) |>
    dplyr::distinct() |>
    dplyr::mutate("obscure_group_2" = 1)
  cols2 <- colnames(groupsToObscure2)[colnames(groupsToObscure2) != "obscure_group_2"]
  result <- result |>
    dplyr::left_join(groupsToObscure1, by = cols1) |>
    dplyr::left_join(groupsToObscure2, by = cols2) |>
    dplyr::mutate(obscure_group = dplyr::case_when(
      obscure_group_2 == 1 &
        !.data$variable_name %in% .env$groupCount &
        !.data$estimate_name %in% .env$estimateName ~ 1,
      obscure_group_1 == 1 &
        !.data$estimate_name %in% .env$estimateName ~ 1,
      TRUE ~ 0
    ))
  return(result)
}
obscureColumn <- function(result, suppressedValue, suppressedGroup) {
  result |>
    dplyr::mutate("estimate_value" = dplyr::case_when(
      .data$obscure_group == 1 ~ .env$suppressedGroup,
      .data$obscure_record == 1 ~ .env$suppressedValue,
      TRUE ~ .data$estimate_value
    )) |>
    dplyr::select(-c("obscure_record", "obscure_group"))
}
