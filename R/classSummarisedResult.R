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

#' 'summarised_results' object constructor
#'
#' @param x input must be a tibble.
#' @param name Name of the summarised result object
#'
#' @return A summarisedResult object
#' @export
#'
summarisedResult <- function(x, name = "summarised_result") {

  # inital input check
  assertTibble(x)
  assertCharacter(name, length = 1, minNumCharacter = 1)

  # constructor
  # TODO convert some columns to sentence case
  x <- newSummarisedResult(x, name)

  # validate
  x <- validateSummariseResult(x)

  return(x)
}

newSummarisedResult <- function(x, name) {

  if (!is.null(x[["result_type"]])) {
    cs <- unique(x[["result_type"]]) |>
      strsplit(split = " and ") |>
      unlist() |>
      unique()
    cs <- c(cs, "summarised_result")
  } else {
    cs <- "summarised_result"
  }

  x <- addClass(x, cs)
  attr(x, "summarised_result_name") <- name

  return(x)
}
validateSummariseResult <- function(x) {
  # compulsory columns
  compulsoryCols <- c(
    "cdm_name", "result_type", "package", "package_version", "group_name",
    "group_level", "strata_name", "strata_level", "variable", "variable_level",
    "variable_type", "estimate_type", "estimate"
  )
  checkColumns(x = x, cols = compulsoryCols)

  # all columns should be character
  checkColumnsFormat(x = x, cols = compulsoryCols, format = "character")

  # Cannot contain NA columns
  notNaCols <- c(
    "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
    "variable", "variable_type", "estimate_type", "estimate"
  )
  checkNA(x = x, cols = notNaCols)

  # Sentence case column
  sentenceCaseCols <- c("result_type")
  checkSentence(x = x, cols = sentenceCaseCols)

  # columPairs
  columnPairs <- c("group_name" = "group_level", "strata_name" = "strata_level")
  checkColumnPairs(x, columnPairs, " and ", "snake")

  return(x)
}
checkColumns <- function(x, cols) {
  if (!all(cols %in% colnames(x))) {
    cli::cli_abort(
      paste0(
        "`",
        paste0(cols, collapse = "`, `"),
        "` must be column names of a summarised_result object."
      )
    )
  }
  invisible(NULL)
}
checkSentence <- function(x, cols) {
  for (col in cols) {
    if (!all(isSentenceCase(unique(x[[col]])))) {
      cli::cli_abort("`{col}` must be in sentence case.")
    }
  }
  invisible(NULL)
}
checkNA <- function(x, cols) {
  for (col in cols) {
    if (all(is.na(unique(x[[col]])))) {
      cli::cli_abort("`{col}` must not contain NA.")
    }
  }
  invisible(NULL)
}
checkColumnsFormat <- function(x, cols, format = "character") {
  if (!all(lapply(x |> dplyr::select(dplyr::all_of(cols)), typeof) |> unlist() == format)) {
    cli::cli_abort(paste0(
      "`",
      paste0(cols, collapse = "`, `"),
      "` must have character format."
    ))
  }
  invisible(NULL)
}
checkColumnPairs <- function(x, pairs, sep, case) {
  for (k in seq_along(pairs)) {
    group <- names(pairs)[k]
    level <- unname(pairs)[k]
    distinctPairs <- x |>
      dplyr::select(
        "group" = dplyr::all_of(group), "level" = dplyr::all_of(level)
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(dplyr::across(
        c("group", "level"),
        list(elements = ~ stringr::str_split(.x, pattern = sep))
      )) |>
      dplyr::mutate(dplyr::across(
        dplyr::ends_with("elements"),
        list(length = ~ lengths(.x))
      ))
    notMatch <- distinctPairs |>
      dplyr::filter(
        .data$group_elements_length != .data$level_elements_length
      )
    if (nrow(notMatch) > 0) {
      unmatch <- notMatch |>
        dplyr::select("group", "level") |>
        dplyr::mutate("group_and_level" = paste0(
          .env$group, ": ", .data$group, "; ", .env$level, ": ", .data$level
        )) |>
        dplyr::pull("group_and_level")
      num <- length(unmatch)
      nun <- min(num, 5)
      unmatch <- unmatch[1:nun]
      names(unmatch) <- rep("*", nun)

      mes <- "group: `{group}` and level: `{level}` does not match in number of
      arguments ({num} unmatch), first {nun} unmatch:"
      cli::cli_abort(c(mes, unmatch))
    }

    groupCase <- distinctPairs[["group_elements"]] |> unlist() |> unique()
    if (!all(isCase(groupCase, case))) {
      cli::cli_abort("elements in {group} are not {case} case")
    }
  }
}
isCase <- function(x, case) {
  flag <- switch(
    case,
    "snake" = isSnakeCase(x),
    "sentence" = isSentenceCase(x),
    "NA" = rep(TRUE, length(x)),
    rep(NA, length(x))
  )
  return(flag)
}
isSentenceCase <- function(x) {
  x == snakecase::to_sentence_case(x)
}
isSnakeCase <- function(x) {
  x == snakecase::to_snake_case(x)
}
