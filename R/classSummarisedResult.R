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

#' 'summarised_results' object constructor
#'
#' @param x Table.
#'
#' @return A summarisedResult object
#'
#' @export
#'
newSummarisedResult <- function(x) {

  # inital input check
  assertClass(x, "data.frame")

  # constructor
  x <- constructSummarisedResult(x)

  # validate
  x <- validateSummariseResult(x)

  return(x)
}

constructSummarisedResult <- function(x) {
  x |> addClass("summarised_result")
}
validateSummariseResult <- function(x) {
  if (!"result_id" %in% colnames(x)) {
    x <- x |> dplyr::mutate("result_id" = as.integer(NA))
    cli::cli_alert_warning(
      "`result_id` column is missing, please add it as it is a compulsory column."
    )
  }

  # compulsory columns
  x <- checkColumns(x = x, "summarised_result")

  # all columns should be character
  x <- checkColumnsFormat(x = x, "summarised_result")

  # Cannot contain NA columns
  checkNA(x = x, "summarised_result")

  # columPairs
  columnPairs <- c(
    "group_name" = "group_level", "strata_name" = "strata_level",
    "additional_name" = "additional_level"
  )
  checkColumnPairs(x, columnPairs, " and | &&& ", "snake")

  # estimate_type
  checkColumnContent(
    x = x, col = "estimate_type", content = estimateTypeChoices()
  )

  return(x)
}
checkColumns <- function(x, resultName) {
  cols <- resultColumns(table = resultName)
  notPresent <- cols[!cols %in% colnames(x)]
  if (length(notPresent) > 0) {
    cli::cli_abort(
      "{paste0(notPresent, collapse = ', ')} must be present in a {resultName}
      object."
    )
  }
  extra <- colnames(x)[!colnames(x) %in% cols]
  if (length(extra) > 0) {
    cli::cli_abort(
      "{paste0(extra, collapse = ', ')} are not allowed column names of a
      {resultName} object."
    )
  }
  x |> dplyr::select(dplyr::all_of(cols))
}
checkNA <- function(x, type) {
  cols <- fieldsResults$result_field_name[
    fieldsResults$result == type & fieldsResults$na_allowed == FALSE
  ]
  for (col in cols) {
    if (any(is.na(unique(x[[col]])))) {
      cli::cli_abort("`{col}` must not contain NA.")
    }
  }
  invisible(NULL)
}
checkColumnsFormat <- function(x, resultName) {
  cols <- resultColumns(resultName)
  expectedFormat <- fieldsResults$datatype[fieldsResults$result == resultName]
  formats <- lapply(x, typeof) |> unlist()
  id <- formats != expectedFormat
  cols <- cols[id]
  formats <- formats[id]
  expectedFormat <- expectedFormat[id]
  if (length(cols) > 0) {
    err <- character()
    for (k in seq_along(cols)) {
      res <- tryCatch(
        expr = {
          x <- x |>
            dplyr::mutate(!!cols[k] := giveType(.data[[cols[k]]], expectedFormat[k]))
          list(x = x, err = character())
        },
        error = function(e) {
          list(x = x, err = cols[k])
        }
      )
      x <- res$x
      err <- c(err, res$err)
    }
    if (length(err) > 0) {
      err <- paste0(err, ": format=", formats, " (expected=", expectedFormat, ")")
      names(err) <- rep("*", length(err))
      cli::cli_abort(c("The following colum does not have a correct format", err))
    } else {
      err <- paste0(cols, ": from ", formats, " to ", expectedFormat)
      names(err) <- rep("*", length(err))
      cli::cli_inform(c("!" = "The following column type were changed:", err))
    }
  }
  invisible(x)
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
  if (length(x) > 0) {
    x == snakecase::to_sentence_case(x)
  } else {
    x
  }
}
isSnakeCase <- function(x) {
  if (length(x) > 0) {
    x == toSnakeCase(x)
  } else {
    x
  }
}
checkColumnContent <- function(x, col, content) {
  if (!all(x[[col]] %in% content)) {
    notType <- x[[col]][!x[[col]] %in% content] |> unique()
    len <- length(notType)
    notType <- notType[1:min(5, len)]
    cli::cli_abort(c(
      "{col} contains incorrect values, possible values:
      {paste0(content, collapse = ', ')}. Observed values:
      {paste0(notType, collapse = ', ')}{ifelse(len>5, '...', '.')}"
    ))
  }
  return(invisible(TRUE))
}
giveType <- function(x, type) {
  switch(
    type,
    "integer" = as.integer(x),
    "double" = as.double(x),
    "character" = as.character(x),
    "logical" = as.logical(x),
    x
  )
}

#' Required columns that the result tables must have.
#'
#' @param table Table to see required columns.
#'
#' @return Required columns
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' resultColumns()
#'
resultColumns <- function(table = "summarised_result") {
  assertChoice(table, unique(fieldsResults$result))
  x <- fieldsResults$result_field_name[fieldsResults$result == table]
  return(x)
}

#' Choices that can be present in `estimate_type` column.
#'
#' @return A character vector with the options that can be present in
#' `estimate_type` column in the summarised_result objects.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' estimateTypeChoices()
#'
estimateTypeChoices <- function() {
  c(
    "numeric", "integer", "date", "character", "proportion", "percentage",
    "logical"
  )
}

#' Empty `summarised_result` object.
#'
#' @return An empty `summarised_result` object.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' emptySummarisedResult()
#'
emptySummarisedResult <- function() {
  resultColumns("summarised_result") |>
    rlang::rep_named(list(character())) |>
    dplyr::as_tibble() |>
    newSummarisedResult()
}
