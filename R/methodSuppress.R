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
#' @export
#'
suppress <- function(result,
                     minCellCount = 5) {
  UseMethod("suppress")
}

#' Function to suppress counts in compared_result objects
#'
#' @param result compared_result object
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table with suppressed counts
#'
#' @export
suppress.compared_result <- function(result,
                                     minCellCount = 5) {

}

#' Function to suppress counts in summarised_result objects
#'
#' @param result summarised_result object
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table with suppressed counts
#'
#' @export
suppress.summarised_result <- function(result,
                                       minCellCount = 5) {
  variable = "estimate_value"
  estimateName = "count"
  group = c("group_name", "group_level", "strata_name", "strata_level")
  groupCount = c("number subjects", "number records")
  linkEstimates = list("count" = "percentage")

  # initial checks
  assertTibble(result)
  assertNumeric(minCellCount, integerish = TRUE, min = 0, length = 1)

  # loop for different columns
  for (col in variable) {
    result <- result |>
      # as numeric
      dplyr::mutate(!!col := suppressWarnings(as.numeric(.data[[col]]))) |>
      # obscure groups flag
      obscureGroups(minCellCount, col, estimateName, group, groupCount) |>
      # obscure records
      obscureRecords(minCellCount, col, estimateName) |>
      # obscure linked
      obscureLinked(linkEstimates, variable) |>
      # obscure col
      obscureColumn(col, minCellCount, groupCount, linkEstimates)
  }
  return(result)
}

filterData <- function(result, minCellCount, variable, estimateName) {
  if (!is.null(estimateName)) {
    result <- result |>
      dplyr::filter(.data$estimate_name %in% .env$estimateName)
  }
  result <- result |>
    dplyr::filter(
      .data[[variable]] < .env$minCellCount & .data[[variable]] > 0
    )
  return(result)
}
obscureGroups <- function(result, minCellCount, variable, estimateName, group, groupCount) {
  result <- result |> dplyr::mutate(obscure_group = 0)
  if (!is.null(group) & all(group %in% colnames(result))) {
    groupsToObscure <- result |>
      dplyr::select(dplyr::all_of(c(group, variable, "variable_name", "estimate_name"))) |>
      dplyr::filter(.data$variable_name %in% .env$groupCount) |>
      filterData(minCellCount, variable, estimateName) |>
      dplyr::select(dplyr::all_of(group)) |>
      dplyr::distinct() |>
      dplyr::mutate(obscure_group = 1)
    result <- result |>
      dplyr::select(-"obscure_group") |>
      dplyr::left_join(groupsToObscure, by = group) |>
      dplyr::mutate(
        obscure_group = dplyr::if_else(is.na(.data$obscure_group), 0, 1)
      )
  }
  return(result)
}
obscureRecords <- function(result, minCellCount, variable, estimateName) {
  recordsToObscure <- result |>
    filterData(minCellCount, variable, estimateName) |>
    dplyr::mutate(obscure_record = 1)
  result <- result |>
    dplyr::left_join(recordsToObscure, by = colnames(result)) |>
    dplyr::mutate(
      obscure_record = dplyr::if_else(is.na(.data$obscure_record), 0, 1)
    )
  return(result)
}
obscureLinked <- function(result, linkEstimates, variable) {
  cols <- colnames(result)
  cols <- cols[!(cols %in% variable)]
  result <- result |>
    dplyr::mutate(obscure_linked = 0)
  for (k in seq_along(linkEstimates)) {
    result <- result |>
      dplyr::left_join(
        result |>
          dplyr::filter(
            .data$estimate_name == names(linkEstimates)[k],
            .data$obscure_record == 1,
            .data$obscure_group != 1
          ) |>
          dplyr::inner_join(
            dplyr::tibble(
              estimate_name = names(linkEstimates)[k],
              new_estimate_name = linkEstimates[[k]]
            ),
            by = "estimate_name"
          ) |>
          dplyr::select(-dplyr::all_of(c("estimate_name", variable, "obscure_linked"))) |>
          dplyr::rename("estimate_name" = "new_estimate_name") |>
          dplyr::mutate(obscure_linked_k = 1),
        by = cols
      ) |>
      dplyr::mutate(obscure_linked = dplyr::if_else(
        .data$obscure_linked_k == 1, 1, .data$obscure_linked
      )) |>
      dplyr::select(-"obscure_linked_k") |>
      dplyr::mutate(obscure_linked = dplyr::if_else(is.na(.data$obscure_linked),
                                                    0,
                                                    .data$obscure_linked))
  }
  return(result)
}
obscureColumn <- function(result, col, minCellCount, groupCount, linkEstimates) {
  minCellCount <- paste0("<", minCellCount)
  result <- result |>
    dplyr::mutate(
      !!col := dplyr::if_else(
        .data$obscure_group == 1,
        dplyr::if_else(
          .data$variable_name %in% .env$groupCount,
          .env$minCellCount,
          as.character(NA)
        ),
        dplyr::if_else(
          .data$obscure_record == 1,
          .env$minCellCount,
          dplyr::if_else(
            .data$obscure_linked == 1,
            as.character(NA),
            as.character(.data[[col]])
          )
        )
      )
    ) |>
    dplyr::select(-c("obscure_record", "obscure_group", "obscure_linked"))
  return(result)
}
