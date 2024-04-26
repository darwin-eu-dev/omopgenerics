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

addClass <- function(x, value) {
  if (any(value %in% class(x))) x <- removeClass(x, value)
  base::class(x) <- c(value, base::class(x))
  return(x)
}
removeClass <- function(x, value) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
getVocabularyVersion <- function(x) {
  vocabVersion <- NULL
  if ("vocabulary" %in% names(x) && "vocabulary_version" %in% colnames(x)) {
    vocabVersion <- x[["vocabulary"]] |>
      dplyr::filter(.data$vocabulary_id == "None") |>
      dplyr::pull(.data$vocabulary_version)
  }
  if (length(vocabVersion) == 0) {
    vocabVersion <- NA_character_
  }
  return(vocabVersion)
}
assertCharacter <- function(x,
                            length = NULL,
                            na = FALSE,
                            null = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a character",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      minNumCharacter > 0,
      paste("; at least", minNumCharacter, "per element"),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {

    # assert class
    if (!is.character(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # minimum number of characters
    if (any(nchar(xNoNa) < minNumCharacter)) {
      cli::cli_abort(errorMessage, call = call)
    }
  }

  return(invisible(x))
}
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       named = FALSE,
                       class = NULL,
                       call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a list",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    ifelse(
      !is.null(class),
      paste("; elements must have class:", paste0(class, collapse = ", ")),
      ""
    ),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {

    # assert class
    if (!is.list(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert class
    if (!is.null(class)) {
      flag <- lapply(xNoNa, function(y) {
        any(class %in% base::class(y))
      }) |>
        unlist() |>
        all()
      if (flag != TRUE) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
assertChoice <- function(x,
                         choices,
                         length = NULL,
                         na = FALSE,
                         null = FALSE,
                         named = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a choice between: ",
    paste0(choices, collapse = ", "),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {

    # assert class
    if (!all(class(x) == class(choices))) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)

    # assert choices
    if (base::length(xNoNa) > 0) {
      if (!all(xNoNa %in% choices)) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
assertLogical <- function(x,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a logical",
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!is.logical(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)
  }

  return(invisible(x))
}
assertNumeric <- function(x,
                          integerish = FALSE,
                          min = -Inf,
                          max = Inf,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a numeric",
    ifelse(integerish, "; it has to be integerish", ""),
    ifelse(is.infinite(min), "", paste0("; greater than", min)),
    ifelse(is.infinite(max), "", paste0("; smaller than", max)),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {

    # assert class
    if (!is.numeric(x)) {
      cli::cli_abort(errorMessage, call = call)
    }

    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert integerish
    if (integerish & base::length(xNoNa) > 0) {
      err <- max(abs(xNoNa - round(xNoNa)))
      if (err > 0.0001) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert lower bound
    if (!is.infinite(min) & base::length(xNoNa) > 0) {
      if (base::min(xNoNa) < min) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert upper bound
    if (!is.infinite(max) & base::length(xNoNa) > 0) {
      if (base::max(xNoNa) > max) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert length
    assertLength(x, length, errorMessage, call)

    # assert na
    assertNa(x, na, errorMessage, call)

    # assert named
    assertNamed(x, named, errorMessage, call)
  }

  return(invisible(x))
}
assertTibble <- function(x,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         null = FALSE,
                         call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a tibble",
    ifelse(is.null(numberColumns), "", paste0("; with at least ", numberColumns, " columns")),
    ifelse(is.null(numberRows), "", paste0("; with at least ", numberRows, " rows")),
    ifelse(is.null(columns), "", paste0("; the following columns must be present: ", paste0(columns, collapse = ", "))),
    errorNull(null),
    "."
  )

  # assert null
  if (assertNull(x, null, errorMessage, call)) {
    # assert class
    if (!("tbl" %in% class(x))) {
      cli::cli_abort(errorMessage, call = call)
    }

    # assert numberColumns
    if (!is.null(numberColumns)) {
      if (length(x) != numberColumns) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert numberRows
    if (!is.null(numberRows)) {
      if (nrow(x) != numberRows) {
        cli::cli_abort(errorMessage, call = call)
      }
    }

    # assert columns
    if (!is.null(columns)) {
      if (!all(columns %in% colnames(x))) {
        cli::cli_abort(errorMessage, call = call)
      }
    }
  }

  return(invisible(x))
}
assertClass <- function(x,
                        class,
                        null = FALSE,
                        call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""), " must have class: ",
    paste0(class, collapse = ", "), "; but has class: ",
    paste0(base::class(x), collapse = ", ") ,"."
  )
  if (is.null(x)) {
    if (null) {
      return(invisible(x))
    } else {
      cli::cli_abort(
        "{paste0(substitute(x), collapse = '')} can not be NULL.", call = call
      )
    }
  }
  if (!all(class %in% base::class(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
assertLength <- function(x, length, errorMessage, call) {
  if (!is.null(length) && base::length(x) != length) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
errorLength <- function(length) {
  if (!is.null(length)) {
    str <- paste0("; with length = ", length)
  } else {
    str <- ""
  }
  return(str)
}
assertNa <- function(x, na, errorMessage, call) {
  if (!na && any(is.na(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
errorNa <- function(na) {
  if (na) {
    str <- ""
  } else {
    str <- "; it can not contain NA"
  }
  return(str)
}
assertNamed <- function(x, named, errorMessage, call) {
  if (named && length(names(x)[names(x) != ""]) != length(x)) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}
errorNamed <- function(named) {
  if (named) {
    str <- "; it has to be named"
  } else {
    str <- ""
  }
  return(str)
}
assertNull <- function(x, null, errorMessage, call) {
  if (!null && is.null(x)) {
    cli::cli_abort(errorMessage, call = call)
  }
  return(!is.null(x))
}
errorNull <- function(null) {
  if (null) {
    str <- ""
  } else {
    str <- "; it can not be NULL"
  }
  return(str)
}

#' Convert a character vector to snake case
#'
#' @param x Character vector to convert
#'
#' @return A snake_case vector
#'
#' @export
#'
#' @examples
#' toSnakeCase("myVariable")
#'
#' toSnakeCase(c("cohort1", "Cohort22b"))
#'
toSnakeCase <- function(x) {
  snakecase::to_snake_case(string = x, numerals = "asis")
}


#' Get the cohort definition id of a certain name
#'
#' @param cohort A cohort_table object.
#' @param cohortName Names of the cohort of interest.
#'
#' @return Cohort definition ids
#'
#' @export
#'
getCohortId <- function(cohort, cohortName) {
  # check inputs
  assertClass(cohort, "cohort_table")
  assertCharacter(cohortName)

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")
  notPresent <- cohortName[!cohortName %in% set$cohort_name]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts names not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  x <- dplyr::tibble("cohort_name" = cohortName) |>
    dplyr::inner_join(set, by = "cohort_name")
  x$cohort_definition_id |> rlang::set_names(x$cohort_name)
}

#' Get the cohort name of a certain cohort definition id
#'
#' @param cohort A cohort_table object.
#' @param cohortId Cohort definition id of interest.
#'
#' @return Cohort names
#'
#' @export
#'
getCohortName <- function(cohort, cohortId) {
  # check inputs
  assertClass(cohort, "cohort_table")
  assertNumeric(cohortId, integerish = TRUE)

  set <- settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name")
  notPresent <- cohortId[!cohortId %in% set$cohort_definition_id]
  if (length(notPresent) > 0) {
    cli::cli_warn(c(
      "!" = "Cohorts definition ids not found: {paste0(notPresent, collapse = ', ')}."
    ))
  }
  x <- dplyr::tibble("cohort_definition_id" = as.integer(cohortId)) |>
    dplyr::inner_join(set, by = "cohort_definition_id")
  x$cohort_name |> rlang::set_names(x$cohort_definition_id)
}
