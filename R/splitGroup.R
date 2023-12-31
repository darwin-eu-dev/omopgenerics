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

#' Split a pair of columns in name-level format to several columns.
#'
#' @param x Tibble or data.frame.
#' @param name Column name of the `name` column.
#' @param level Column name of the `level` column.
#' @param keep Whether to keep the original columns.
#'
#' @return A Tibble with the new columns.
#'
#' @export
#'
splitGroup <- function(x,
                       name = "group_name",
                       level = "group_level",
                       keep = FALSE) {
  # initial checks
  assertCharacter(name, length = 1)
  assertCharacter(level, length = 1)
  assertLogical(keep, length = 1)
  assertTibble(x, columns = c(name, level))

  nameValues <- x[[name]] |> stringr::str_split(" and ")
  levelValues <- x[[level]] |> stringr::str_split(" and ")
  if (!all(lengths(nameValues) == lengths(levelValues))) {
    cli::cli_abort("Column names and levels number does not match")
  }

  nameValue <- unique(unlist(nameValues))
  present <- nameValue[nameValue %in% colnames(x)]
  if (length(present) > 0) {
    cli::cli_warn(
      "The following columns will be overwritten:
      {paste0(present, collapse = ', ')}."
    )
  }
  for (k in seq_along(nameValue)) {
    col <- nameValue[k]
    dat <- lapply(seq_along(nameValues), function(y) {
      res <- levelValues[[y]][nameValues[[y]] == col]
      if (length(res) == 0) {
        return(as.character(NA))
      } else {
        return(res)
      }
    }) |>
      unlist()
    x[[col]] <- dat
  }

  if (!keep) {
    x <- x |> dplyr::select(-dplyr::all_of(c(name, level)))
  }

  return(x)
}
