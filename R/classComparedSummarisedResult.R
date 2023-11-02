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
  assertTibble(x)

  #constructer
  x <- newComparedResult(x)


  # validate
  x <- validateComparedResult(x)



  return(x)
}


newComparedResult <- function(x) {

  class(x) <- c("compared_result", class(x))


  return(x)
}


validateComparedResult <- function(x) {

  # class
  if (!"compared_result" %in% class(x)) {
    cli::cli_abort("The tibble does not has the summarised_result class")
  }

  # compulsory columns
  compulsoryCols <- c(
    "cdm_name",
    "result_type",
    "package",
    "package_version",
    "group_name_reference",
    "group_level_reference",
    "strata_name_reference",
    "strata_level_reference",
    "group_name_comparator",
    "group_level_comparator",
    "strata_name_comparator",
    "strata_level_comparator",
    "variable",
    "variable_level",
    "variable_type",
    "estimate_type",
    "estimate"
  )

  # Cannot contain NA columns
  notNaCols <- c(
    "cdm_name",
    "result_type",
    "package",
    "package_version",
    "group_name_reference",
    "group_level_reference",
    "group_name_comparator",
    "group_level_comparator",
    "variable",
    "variable_level",
    "variable_type",
    "estimate_type",
    "estimate"
  )

  #Sentence case column
  sentenceCaseCols <- c("result_type")

  #snake case column
  snakeCaseCols <- c("group_name_reference", "group_name_comparator")

  # assert columns
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

  checkColumns(
    x = x,
    cols = compulsoryCols
  )

  # assert format of column is character
  checkColumnsFormat <- function(x, cols, format = "character") {
    if (!all(lapply(x |> dplyr::select(cols), typeof) |> unlist() == format)) {
      cli::cli_abort(paste0(
        "`",
        paste0(cols, collapse = "`, `"),
        "` must have character format."
      ))
    }
    invisible(NULL)
  }

  checkColumnsFormat(x = x, cols = compulsoryCols, format = "character")

  # assert column cannot contain NA
  checkNA <- function(x, cols) {
    if (!all(apply(x |> dplyr::select(cols), 2, function(x)
      is.na(x)) == FALSE)) {
      cli::cli_abort(paste0("`",
                            paste0(cols, collapse = "`, `"),
                            "` must not contain NA."))
    }
    invisible(NULL)


  }

  checkNA(x = x, cols = notNaCols)

  # assert snake case column
  checkSnake <- function(x, cols) {
    x <- x |> dplyr::select(cols) |> unlist() |> unique()

    y <-
      x |> snakecase::to_snake_case()

    if (!all(x == y)) {
      cli::cli_abort(paste0("`",
                            paste0(cols, collapse = "`, `"),
                            "` must be in snakecase."))

    }
    invisible(NULL)
  }

  checkSnake(x = x, cols = snakeCaseCols)

  # assert sentence case column
  checkSentence <- function(x, cols) {
    x <- x |> dplyr::select(cols) |> unlist() |> unique()

    y <-
      x |> snakecase::to_sentence_case()

    if (!all(x == y)) {
      cli::cli_abort(paste0(
        "`",
        paste0(cols, collapse = "`, `"),
        "` must be in sentence case."
      ))

    }
    invisible(NULL)
  }

  checkSentence(x = x, cols = sentenceCaseCols)


  return(x)

}
