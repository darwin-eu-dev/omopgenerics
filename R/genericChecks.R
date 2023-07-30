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

#' @noRd
checkCharacter <- function(x,
                           named = FALSE,
                           len = NULL,
                           error) {
  if (!is.list(x)) {
    cli::cli_abort(error)
  }
  if (named == TRUE & length(names(x)) != length(x)) {
    cli::cli_abort(error)
  }
  if (!is.null(len) & length(x) != len) {
    cli::cli_abort(error)
  }
}

#' @noRd
checkList <- function(x,
                      named = FALSE,
                      types = NULL,
                      error,
                      uniqueType = TRUE,
                      len = NULL) {
  if (!is.list(x)) {
    cli::cli_abort(error)
  }
  if (named == TRUE & length(names(x)) != length(x)) {
    cli::cli_abort(error)
  }
  if (!is.null(len) & length(x) != len) {
    cli::cli_abort(error)
  }
  if (!is.null(types)) {
    flag <- lapply(x, function(y) {
      any(types %in% class(y))
    }) %>%
      any()
    if (flag != TRUE) {
      cli::cli_abort(error)
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
      cli::cli_abort(error)
    }
  }
}

#' @noRd
checkChoice <- function(x,
                        choices,
                        error,
                        len = 1,
                        null.ok = FALSE) {
  if (!(null.ok == TRUE & is.null(x))) {
    if (length(x) != len) {
      cli::cli_abort(error)
    }
    if (!all(sort(unique(class(x)))) == sort(unique(class(choices)))) {
      cli::cli_abort(error)
    }
    if (!all(x %in% choices)) {
      cli::cli_abort(error)
    }
  }
}

#' @noRd
checkLogical <- function(x, len = 1, na.ok = FALSE, error) {
  if (!is.logical(x)) {
    cli::cli_abort(error)
  }
  if (!is.null(len) && length(x) != len) {
    cli::cli_abort(error)
  }
  if (!na.ok && any(is.na(x))) {
    cli::cli_abort(error)
  }
}
