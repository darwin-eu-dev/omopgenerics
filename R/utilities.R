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

#' Convert a character to snake_case
#'
#' @param string String to convert to snake_case
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMUtilities)
#' toSnakeCase(c("foo bar", "fooBar", "FooBar", "foo_bar", "FOOBAR"))
#' }
#'
toSnakeCase <- function(string) {
  # checkInput
  checkInput(string = string)

  # eliminate special characters
  string <- gsub("[^[:alnum:]]", "_", string)

  # zero lengths
  string[nchar(string) == 0] <- as.character(NA)

  # identify upper case
  string <- strsplit(string, "")
  string <- lapply(string, function(x) {
    if (length(x) > 1) {
      id <- grepl("[A-Z]", x)
      addUnderscore <- which(id & c(FALSE, !id[1:length(id)-1]))
      if (length(addUnderscore) != 0) {
        addUnderscore <- addUnderscore + seq_along(addUnderscore) - 1
        y <- character(length(x) + length(addUnderscore))
        y[addUnderscore] <- "_"
        y[-addUnderscore] <- x
      } else {
        y <- x
      }
      x <- paste0(tolower(y), collapse = "")
    }
    return(x)
  }) %>%
    unlist()

  # eliminate "__"
  while(any(grepl("__", string))) {
    string <- gsub("__", "_", string)
  }

  # zero lengths
  string[is.na(string)] <- ""

  # eliminate initial/final "_"
  string <- sapply(string, function(x) {
    if (nchar(x) > 1 && substr(x, 1, 1) == "_") {
      x <- substr(x, 2, nchar(x))
    }
    if (nchar(x) > 1 && substr(x, nchar(x), nchar(x)) == "_") {
      x <- substr(x, 1, nchar(x)-1)
    }
    return(x)
  })
  names(string) <- NULL

  return(string)
}

#' Convert a character to camelCase
#'
#' @param string String to convert to camelCase
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMUtilities)
#' toCamelCase(c("foo bar", "fooBar", "FooBar", "foo_bar", "FOOBAR"))
#' }
#'
toCamelCase <- function(string) {
  # first to snake case
  string <- toSnakeCase(string)

  # zero lengths
  string[nchar(string) == 0] <- as.character(NA)

  # add upper case
  string <- lapply(strsplit(string, ""), function(x) {
    if (length(x) > 1) {
      id <- which(x == "_") + 1
      x[id] <- toupper(x[id])
      x <- paste0(gsub("_", "", x), collapse = "")
    }
    return(x)
  }) %>%
    unlist()
  string[is.na(string)] <- ""
  names(string) <- NULL

  return(string)
}

#' Set temporary (or permanent) behavior for compute functions.
#'
#' @param intermediateAsTemp Whether intermediate tables should be temporary.
#' @param cohortAsTemp Whether cohort tables should be temporary.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMUtilities)
#' setTemporary(intermediateAsTemp = TRUE, cohortAsTemp = FALSE)
#' }
#'
setTemporary <- function(intermediateAsTemp = TRUE,
                         cohortAsTemp = FALSE) {
  # check inputs
  checkInput(
    intermediateAsTemp = intermediateAsTemp, cohortAsTemp = cohortAsTemp
  )

  # set options
  options("intermediate_as_temp" = intermediateAsTemp)
  options("cohort_as_temp" = cohortAsTemp)

  # return
  return(invisible(NULL))
}

asType <- function(x, type) {
  if (type == "integer") {
    x <- as.integer(x)
  } else if (grepl("date", type)) {
    x <- as.Date(x)
  } else if (type == "float") {
    x <- as.numeric(x)
  } else if (grepl("varchar", type)) {
    x <- as.character(x)
  } else {
    displayWarningMessage(paste0("Not recognised type: ", type))
  }
  return(x)
}
