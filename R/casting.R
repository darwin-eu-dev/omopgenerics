# Copyright 2024 DARWIN EU (C)
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

castColumns <- function(table, cols, name) {
  colsToCast <- detectColsToCast(table, cols)
  if (length(colsToCast$new) > 0) {
    warnColsToCast(colsToCast, name)
    table <- castTableColumns(table, colsToCast)
  }
  return(table)
}
detectColsToCast <- function(table, cols) {
  colTypes <- table |>
    utils::head(1) |>
    dplyr::collect() |>
    lapply(dplyr::type_sum) |>
    lapply(assertClassification)
  vals <- intersect(names(colTypes), names(cols))
  differentValues <- vals[unlist(cols[vals]) != unlist(colTypes[vals])]
  colsToCast <- list(
    "new" = cols[differentValues], "old" = colTypes[differentValues])
  return(colsToCast)
}
warnColsToCast <- function(colsToCast, name) {
  msg <- NULL
  nms <- names(colsToCast$new)
  for (nm in nms) {
    msg <- c(msg, "*" = paste0(
      "`", nm, "` from {.pkg ", colsToCast$old[[nm]], "} to {.pkg ",
      colsToCast$new[[nm]], "}"))
  }
  msg <- c("!" = "{length(colsToCast$new)} casted column{?s} in {.strong {name}} as do not match expected column type:", msg)
  cli::cli_warn(message = msg)
}
castTableColumns <- function(table, colsToCast) {
  cols <- colsToCast$new |> funToCast()
  qC <- paste0(cols, "(.data[['", names(cols), "']])") |>
    rlang::parse_exprs() |>
    rlang::set_names(names(cols))
  table <- table |> dplyr::mutate(!!!qC)
  return(table)
}
funToCast <- function(x) {
  x[x == "integer"] <- "as.integer"
  x[x == "character"] <- "as.character"
  x[x == "date"] <- "as.Date"
  x[x == "numeric"] <- "as.numeric"
  x[x == "logical"] <- "as.logicla"
  return(x)
}
