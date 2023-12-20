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

#' Unite one or more columns in name-level format.
#'
#' @param x Tibble or data.frame.
#' @param cols Columns to aggregate.
#' @param name Column name of the `name` column.
#' @param level Column name of the `level` column.
#' @param keep Whether to keep the original columns.
#'
#' @return A Tibble with the new columns.
#'
#' @export
#'
uniteGroup <- function(x,
                       cols,
                       name = "group_name",
                       level = "group_level",
                       keep = FALSE) {
  # initial checks
  assertCharacter(cols)
  assertCharacter(name, length = 1)
  assertCharacter(level, length = 1)
  assertLogical(keep, length = 1)
  assertTibble(x, columns = cols)

  present <- c(name, level)[c(name, level) %in% colnames(x)]
  if (length(present) > 0) {
    cli::cli_warn(
      "The following columns will be overwritten:
      {paste0(present, collapse = ', ')}."
    )
  }

  containAnd <- cols[grepl(" and ", cols)]
  if (length(containAnd) > 0) {
    cli::cli_abort("Column names must not contain ' and ' : `{paste0(containAnd, collapse = '`, `')}`")
  }
  containAnd <- cols[
    lapply(cols, function(col){any(grepl(" and ", x[[col]]))}) |> unlist()
  ]
  if (length(containAnd) > 0) {
    cli::cli_abort("Column values must not contain ' and '. Present in: `{paste0(containAnd, collapse = '`, `')}`.")
  }

  originalCols <- colnames(x)
  if (!keep) {
    originalCols <- originalCols[!originalCols %in% cols]
  }

  x |>
    dplyr::mutate(!!name := paste0(cols, collapse = " and ")) |>
    tidyr::unite(
      col = !!level, dplyr::all_of(cols), sep = " and ", remove = !keep
    ) |>
    dplyr::select(dplyr::all_of(c(originalCols, name, level)))
}
