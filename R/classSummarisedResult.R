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

# NA
# classes
# colnaming
# sentence
# snake (only pair group)

#' 'summarised_results' object constructor
#'
#' @param x Table.
#'
#' @return A summarisedResult object
#' @export
#'
summarisedResult <- function(x) {

  # inital input check
  assertClass(x, "data.frame")

  # constructor
  x <- newSummarisedResult(x)

  # validate
  x <- validateSummariseResult(x)

  return(x)
}

newSummarisedResult <- function(x) {
  x <- getClass(x, "summarised_result")
  return(x)
}
validateSummariseResult <- function(x) {
  # compulsory columns
  x <- checkColumns(x = x, "summarised_result")

  # all columns should be character
  checkColumnsFormat(x = x, "summarised_result")

  # Cannot contain NA columns
  notNaCols <- c(
    "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
    "variable_name", "variable_type", "estimate_name", "estimate_type",
    "additional_name", "additional_level"
  )
  checkNA(x = x, cols = notNaCols)

  checkResultType(x = x)

  # columPairs
  columnPairs <- c(
    "group_name" = "group_level", "strata_name" = "strata_level",
    "additional_name" = "additional_level"
  )
  checkColumnPairs(x, columnPairs, " and ", "snake")

  # estimate_type
  checkColumnContent(
    x = x, col = "estimate_type", content = c(
      "numeric", "integer", "date", "character", "proportion", "percentage",
      "logical"
    )
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
checkResultType <- function(x) {
  x <- unique(x$result_type) |> strsplit(split = " and ") |> unlist() |> unique()
  x <- x[!isSnakeCase(x)]
  x <- x[!is.na(x)]
  if (length(x) > 0) {
    err <- paste0(x, collapse = '\', \'')
    cli::cli_abort(
      "result_type must contain only snake case characters separated by ` and `
      (if multiple). Incorrect format: '{err}'."
    )
  }
}
checkNA <- function(x, cols) {
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
    err <- paste0(cols, ": format=", formats, " (expected=", expectedFormat, ")")
    names(err) <- rep("*", length(err))
    cli::cli_abort(c("The following cols does not have a correct format", err))
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
getClass <- function(x, def) {
  if (!is.null(x[["result_type"]])) {
    cs <- unique(x[["result_type"]]) |>
      strsplit(split = " and ") |>
      unlist() |>
      unique()
    cs <- cs[!is.na(cs)]
    cs <- c(cs, def)
  } else {
    cs <- def
  }
  x <- addClass(x, cs)
  return(x)
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

#' Required columns that the result tables must have.
#'
#' @param table Table to see required columns.
#'
#' @return Required columns
#'
#' @export
#'
resultColumns <- function(table) {
  assertChoice(table, unique(fieldsResults$result))
  fieldsResults$result_field_name[fieldsResults$result == table]
}

#' Subset a summarised_result or compared_result object to a certain result_type.
#'
#' @param result A result object.
#' @param resultType A result type identifier.
#'
#' @return A subsetted
#'
subsetResult <- function(result, resultType) {
  # initial check
  if (any(c("summarised_result", "compared_result") %in% class(result))) {
    cli::cli_abort(
      "result object is not a valid summarised_result ot compared_result object"
    )
  }
  assertCharacter(resultType)

  # subset
  x <- result$result_type |> strsplit(split = " and ")
  result <- result |> dplyr::filter(grepl(resultType, x))

  if ("summarised_result" %in% class(result)) {
    result <- summarisedResult(result)
  } else {
    result <- comparedResult(result)
  }

  return(result)
}

# checkSentence <- function(x, cols) {
#   for (col in cols) {
#     notCase <- unique(x[[col]])
#     notCase <- notCase[!isSentenceCase(notCase)]
#     if (length(notCase) > 0) {
#       cli::cli_abort(
#         "`{col}` must be in sentence case. Not sentence case values:
#         {paste0(notCase[1:min(5, length(notCase))], collapse = ', ')}{ifelse(length(notCase)>5, '...', '.')}"
#       )
#     }
#   }
#   invisible(NULL)
# }
# checkSnake <- function(x, cols) {
#   for (col in cols) {
#     notCase <- unique(x[[col]])
#     notCase <- notCase[!isSnakeCase(notCase)]
#     if (length(notCase) > 0) {
#       cli::cli_abort(
#         "`{col}` must be in snake case. Not snake case values:
#         {paste0(notCase[1:min(5, length(notCase))], collapse = ', ')}{ifelse(length(notCase)>5, '...', '.')}"
#       )
#     }
#   }
#   invisible(NULL)
# }
