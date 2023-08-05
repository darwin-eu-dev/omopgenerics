# Copyright 2023 DARWIN EU (C)
#
# This file is part of CDMUtilities
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

# TODO export this checks as generic functions in the future

#' Assert if a character fulfill certain conditions.
#'
#' @param x To check.
#' @param len Length that has to have.
#' @param missing Whether it can contain missing.
#' @param nullOk Whether it can be null.
#' @param named Whether it has to be named.
#' @param minNumCharacter Minimum number of characters.
#' @param errorMessage Error message to display.
#'
#' @export
#'
assertCharacter <- function(x,
                            len = NULL,
                            missing = FALSE,
                            nullOk = FALSE,
                            named = FALSE,
                            minNumCharacter = 0,
                            errorMessage = NULL) {
  # create error message
  if (is.null(errorMessage)) {
    errorMessage <- paste0(
      paste0(substitute(x), collapse = ""),
      " must be a character",
      ifelse(!is.null(len), paste0(errorMessage, ", with length = ", len), ""),
      ifelse(!missing, ", it can not contain missings", ""),
      ifelse(named, ", it has to be named", ""),
      ifelse(
        minNumCharacter > 0,
        paste(", at least", minNumCharacter, "per element"),
        ""
      ),
      "."
    )
  }

  # assert null
  if (assertNull(x, nullOk, errorMessage)) {
    # assert class
    if (!is.character(x)) {
      cli::cli_abort(errorMessage)
    }

    # assert length
    assertLength(x, len, errorMessage)

    # assert missing
    assertMissing(x, missing, errorMessage)

    # assert named
    assertNamed(x, named, errorMessage)

    # minimum number of characters
    if (any(nchar(x[!is.na(x)]) < minNumCharacter)) {
      cli::cli_abort(errorMessage)
    }
  }

  invisible(named)
}

#' @noRd
assertList <- function(x,
                       named = FALSE,
                       types = NULL,
                       errorMessage,
                       uniqueType = TRUE,
                       len = NULL) {
  if (!is.list(x)) {
    cli::cli_abort(errorMessage)
  }
  if (named == TRUE & length(names(x)) != length(x)) {
    cli::cli_abort(errorMessage)
  }
  if (!is.null(len) & length(x) != len) {
    cli::cli_abort(errorMessage)
  }
  if (!is.null(types)) {
    flag <- lapply(x, function(y) {
      any(types %in% class(y))
    }) %>%
      any()
    if (flag != TRUE) {
      cli::cli_abort(errorMessage)
    }
  }
  if (uniqueType == TRUE & length(x) > 1) {
    cl <- class(x[[1]])
    flag <- lapply(x, function(y) {
      cly <- class(y)
      all(cl %in% cly) & length(cl) == length(cly)
    }) %>%
      all()
    if (flag != TRUE) {
      cli::cli_abort(errorMessage)
    }
  }
}

#' @noRd
assertChoice <- function(x,
                         choices,
                         errorMessage,
                         len = 1,
                         null.ok = FALSE) {
  if (!(null.ok == TRUE & is.null(x))) {
    if (length(x) != len) {
      cli::cli_abort(errorMessage)
    }
    if (!all(sort(unique(class(x))) == sort(unique(class(choices))))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(x %in% choices)) {
      cli::cli_abort(errorMessage)
    }
  }
}

#' @noRd
assertLogical <- function(x, len = 1, na.ok = FALSE, errorMessage) {
  if (!is.logical(x)) {
    cli::cli_abort(errorMessage)
  }
  if (!is.null(len) && length(x) != len) {
    cli::cli_abort(errorMessage)
  }
  if (!na.ok && any(is.na(x))) {
    cli::cli_abort(errorMessage)
  }
}

assertLength <- function(x, len, errorMessage) {
  if (!is.null(len) && length(x) != len) {
    cli::cli_abort(errorMessage)
  }
  invisible(x)
}
assertMissing <- function(x, missing, errorMessage) {
  if (!missing && any(is.na(x))) {
    cli::cli_abort(errorMessage)
  }
  invisible(x)
}
assertNamed <- function(x, named, errorMessage) {
  if (named && length(names(x)[names(x) != ""]) != length(x)) {
    cli::cli_abort(errorMessage)
  }
  invisible(x)
}
assertNull <- function(x, nullOk, errorMessage) {
  if (!nullOk && is.null(x)) {
    cli::cli_abort(errorMessage)
  }
  return(!is.null(x))
}
