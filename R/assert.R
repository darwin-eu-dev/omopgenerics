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

#' Assert that an object is a character and fulfill certain conditions.
#'
#' @param x Variable to check.
#' @param length Required length.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be NULL.
#' @param unique Whether it has to contain unique elements.
#' @param named Whether it has to be named.
#' @param minNumCharacter Minimum number of characters.
#' @param call Call argument that will be passed to `cli` error message.
#' @param msg Custom error message.
#'
#' @export
#'
assertCharacter <- function(x,
                            length = NULL,
                            na = FALSE,
                            null = FALSE,
                            unique = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            call = parent.frame(),
                            msg = NULL) {
  nm <- paste0(substitute(x), collapse = "")
  if (is.null(msg)) {
    msg <- errorMessage(
      nm = nm, object = "a character vector", length = length, na = na,
      null = null, unique = unique, named = named,
      minNumCharacter = minNumCharacter
    )
  }

  # assert null
  if (assertNull(x, nm, null, msg, call)) {
    # assert class
    if (!is.character(x)) {
      c("!" = "{.strong {nm} is not a character vector.}", msg) |>
        cli::cli_abort(call = call)
    }

    # assert length
    assertLength(x, nm, length, msg, call)

    # assert na
    assertNa(x, nm, na, msg, call)

    # assert unique
    assertUnique(x, nm, unique, msg, call)

    # assert named
    assertNamed(x, nm, named, msg, call)

    # minimum number of characters
    pos <- which(nchar(x) < minNumCharacter)
    if (length(pos) > 0) {
      c("!" = "{.strong {nm} has less than {minNumCharacter} character{?s} in position: {pos}.}", msg) |>
        cli::cli_abort(call = call)
    }
  }

  return(invisible(x))
}

#' Assert that an object is within a certain oprtions.
#'
#' @param x Variable to check.
#' @param choices Options that x is allowed to be.
#' @param length Required length.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be NULL.
#' @param unique Whether it has to contain unique elements.
#' @param named Whether it has to be named.
#' @param call Call argument that will be passed to `cli` error message.
#'
#' @export
#'
assertChoice <- function(x,
                         choices,
                         length = NULL,
                         na = FALSE,
                         null = FALSE,
                         unique = FALSE,
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
    errorUnique(unique),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, msg, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!all(class(x) == class(choices))) {
      cli::cli_abort(msg, call = call)
    }

    # assert length
    assertLength(x, length, msg, call)

    # assert na
    assertNa(x, na, msg, call)

    # assert unique
    assertUnique(x, unique, msg, call)

    # assert named
    assertNamed(x, named, msg, call)

    # assert choices
    if (base::length(xNoNa) > 0) {
      if (!all(xNoNa %in% choices)) {
        cli::cli_abort(msg, call = call)
      }
    }
  }

  return(invisible(x))
}

#' Assert that an object has a certain class.
#'
#' @param x To check.
#' @param class Expected class or classes.
#' @param null Whether it can be NULL.
#' @param all Whether it should have all the classes or only at least one of
#' them.
#' @param extra Whether the object can have extra classes.
#' @param call Call argument that will be passed to `cli`.
#'
#' @export
#'
assertClass <- function(x,
                        class,
                        null = TRUE,
                        all = FALSE,
                        extra = TRUE,
                        call = parent.frame()) {
  if (null & is.null(x)) {
    return(invisible(x))
  }
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""), " must have class: ",
    paste0(class, collapse = ", "), "; but has class: ",
    paste0(base::class(x), collapse = ", ") ,"."
  )
  if (!all(class %in% base::class(x))) {
    cli::cli_abort(errorMessage, call = call)
  }
  invisible(x)
}

#' Assert that an object is a list.
#'
#' @param x Variable to check.
#' @param length Required length.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be NULL.
#' @param unique Whether it has to contain unique elements.
#' @param named Whether it has to be named.
#' @param class Class that the elements must have.
#' @param call Call argument that will be passed to `cli` error message.
#'
#' @export
#'
assertList <- function(x,
                       length = NULL,
                       na = FALSE,
                       null = FALSE,
                       unique = FALSE,
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
  if (assertNull(x, null, msg, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.list(x)) {
      cli::cli_abort(msg, call = call)
    }

    # assert length
    assertLength(x, length, msg, call)

    # assert na
    assertNa(x, na, msg, call)

    # assert unique
    assertUnique(x, unique, msg, call)

    # assert named
    assertNamed(x, named, msg, call)

    # assert class
    if (!is.null(class)) {
      flag <- lapply(xNoNa, function(y) {
        any(class %in% base::class(y))
      }) |>
        unlist() |>
        all()
      if (flag != TRUE) {
        cli::cli_abort(msg, call = call)
      }
    }
  }

  return(invisible(x))
}

#' Assert that an object is a logical.
#'
#' @param x Variable to check.
#' @param length Required length.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be NULL.
#' @param named Whether it has to be named.
#' @param call Call argument that will be passed to `cli` error message.
#'
#' @export
#'
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
  if (assertNull(x, null, msg, call)) {
    # assert class
    if (!is.logical(x)) {
      cli::cli_abort(msg, call = call)
    }

    # assert length
    assertLength(x, length, msg, call)

    # assert na
    assertNa(x, na, msg, call)

    # assert named
    assertNamed(x, named, msg, call)
  }

  return(invisible(x))
}

#' Assert that an object is a numeric.
#'
#' @param x Variable to check.
#' @param integerish Whether it has to be an integer
#' @param min Minimum value that the object can be.
#' @param max Maximum value that the object can be.
#' @param length Required length.
#' @param na Whether it can contain NA values.
#' @param null Whether it can be NULL.
#' @param unique Whether it has to contain unique elements.
#' @param named Whether it has to be named.
#' @param call Call argument that will be passed to `cli` error message.
#'
#' @export
#'
assertNumeric <- function(x,
                          integerish = FALSE,
                          min = -Inf,
                          max = Inf,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          unique = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a numeric",
    ifelse(integerish, "; it has to be integerish", character()),
    ifelse(is.infinite(min), character(), paste0("; greater than", min)),
    ifelse(is.infinite(max), character(), paste0("; smaller than", max)),
    errorLength(length),
    errorNa(na),
    errorNull(null),
    errorUnique(unique),
    errorNamed(named),
    "."
  )

  # assert null
  if (assertNull(x, null, msg, call)) {
    # no NA vector
    xNoNa <- x[!is.na(x)]

    # assert class
    if (!is.numeric(x)) {
      cli::cli_abort(msg, call = call)
    }

    # assert integerish
    if (integerish & base::length(xNoNa) > 0) {
      err <- max(abs(xNoNa - round(xNoNa)))
      if (err > 0.0001) {
        cli::cli_abort(msg, call = call)
      }
    }

    # assert lower bound
    if (!is.infinite(min) & base::length(xNoNa) > 0) {
      if (base::min(xNoNa) < min) {
        cli::cli_abort(msg, call = call)
      }
    }

    # assert upper bound
    if (!is.infinite(max) & base::length(xNoNa) > 0) {
      if (base::max(xNoNa) > max) {
        cli::cli_abort(msg, call = call)
      }
    }

    # assert length
    assertLength(x, length, msg, call)

    # assert na
    assertNa(x, na, msg, call)

    # assert unique
    assertUnique(x, unique, msg, call)

    # assert named
    assertNamed(x, named, msg, call)
  }

  return(invisible(x))
}

#' Assert that an object is a table.
#'
#' @param x Variable to check.
#' @param numberColumns Number of columns that it has to contain.
#' @param numberRows Number of rows that it has to contain.
#' @param columns Name of the columns required.
#' @param allowExtraColumns Whether extra columns are allowed.
#' @param null Whether it can be NULL.
#' @param distinct Whether it has to contain distinct rows.
#' @param requireLocal Whether the table has to be a data.frame.
#' @param call Call argument that will be passed to `cli` error message.
#'
#' @export
#'
assertTable <- function(x,
                        numberColumns = NULL,
                        numberRows = NULL,
                        columns = NULL,
                        allowExtraColumns = TRUE,
                        null = FALSE,
                        distinct = FALSE,
                        requireLocal = FALSE,
                        call = parent.frame()) {
  # create error message
  errorMessage <- paste0(
    paste0(substitute(x), collapse = ""),
    " must be a tibble",
    ifelse(is.null(numberColumns), character(), paste0("; with at least ", numberColumns, " columns")),
    ifelse(is.null(numberRows), character(), paste0("; with at least ", numberRows, " rows")),
    ifelse(is.null(columns), character(), paste0("; the following columns must be present: ", paste0(columns, collapse = ", "))),
    errorNull(null),
    ifelse(distinct, "; it has to contain distinct rows", character()),
    "."
  )

  # assert null
  if (assertNull(x, null, msg, call)) {
    # assert class
    if (!("tbl" %in% class(x))) {
      cli::cli_abort(msg, call = call)
    }

    # assert numberColumns
    if (!is.null(numberColumns)) {
      if (length(x) != numberColumns) {
        cli::cli_abort(msg, call = call)
      }
    }

    # assert numberRows
    if (!is.null(numberRows)) {
      if (nrow(x) != numberRows) {
        cli::cli_abort(msg, call = call)
      }
    }

    # assert columns
    if (!is.null(columns)) {
      if (!all(columns %in% colnames(x))) {
        cli::cli_abort(msg, call = call)
      }
    }

    # assert distinct
    if (distinct) {
      if (nrow(x) != x |> dplyr::distinct() |> nrow()) {
        cli::cli_abort(msg, call = call)
      }
    }

  }

  return(invisible(x))
}

errorMessage <- function(nm,
                         object,
                         length = NULL,
                         na = NULL,
                         named = NULL,
                         unique = NULL,
                         null = NULL,
                         min = NULL,
                         max = NULL,
                         minNumCharacter = NULL,
                         numberColumns = NULL,
                         numberRows = NULL,
                         columns = NULL,
                         allowExtraColumns = NULL,
                         distinct = NULL) {
  paste0(
    c(
      "{nm} must be {object}",
      if (!is.null(length)) "with length = {length}",
      if (isFALSE(na)) "it can not contain NA",
      if (isTRUE(named)) "it has to be named",
      if (isTRUE(unique)) "it has to contain unique elements",
      if (isFALSE(null)) "it can not be NULL",
      if (is.numeric(min)) "greater or equal to {min}",
      if (is.numeric(max)) "smaller or equal to {max}",
      if (minNumCharacter > 0) "with at least {minNumCharacter} character per element",
      if (is.numeric(numberColumns)) "with exactly {numberColumns} columns",
      if (is.numeric(numberRows)) "with exactly {numberRows} rows",
      if (is.character(columns) && length(columns) > 0) "must contain {columns} as columns",
      if (isFALSE(allowExtraColumns)) "no extra columns are allowed",
      if (isTRUE(distinct)) "rows must be unique"
    ),
    collapse = "; "
  ) |>
    cli::cli_text() |>
    cli::cli_fmt() |>
    paste0(collapse = " ") |>
    paste0(".") |>
    as.character()
}
assertLength <- function(x, nm, length, msg, call) {
  len <- base::length(x)
  if (!is.null(length) && len != length) {
    c(
      "!" = "{.strong `{nm}` has length {len}, but must have length {length}.}",
      msg
    ) |>
      cli::cli_abort(call = call)
  }
  invisible(NULL)
}
assertNa <- function(x, nm, na, msg, call) {
  if (!na && length(x) > 0) {
    pos <- which(is.na(x))
    if (length(pos) > 0) {
      c("!" = "{.strong `{nm}` contains NA in position {pos}.}", msg) |>
        cli::cli_abort(call = call)
    }
  }
  invisible(NULL)
}
assertNamed <- function(x, nm, named, msg, call) {
  if (named && length(names(x)[names(x) != ""]) != length(x)) {
    c("!" = "{.strong `{nm}` must be named.}", msg) |>
      cli::cli_abort(call = call)
  }
  invisible(NULL)
}
assertUnique <- function(x, nm, unique, msg, call) {
  if (unique && length(unique(x)) != length(x)) {
    c("!" = "{.strong `{nm}` must be unique.}", msg) |>
      cli::cli_abort(call = call)
  }
  invisible(NULL)
}
assertNull <- function(x, nm, null, msg, call) {
  if (!null && is.null(x)) {
    c("!" = "{.strong `{nm}` can not be NULL.}", msg) |>
      cli::cli_abort(call = call)
  }
  return(!is.null(x))
}
