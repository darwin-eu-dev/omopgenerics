# Copyright 2023 DARWIN EU (C)
#
# This file is part of OMOPUtilities
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

#' `results_collection` objects constructor
#'
#' @param ... Tibble as result objects.
#'
#' @return A `results_collection` object.
#'
#' @export
#'
resultCollection <- function(...) {
  result <- list(...)
  if (length(result) == 1 & is.null(names(result))) {
    result <- ..1
  }
  # initial input check
  checkInput(result = result)

  result <- newResultCollection(result)

  result <- validateResultCollection(result)

  return(result)
}

newResultCollection <- function(result) {
  class(result) <- "result_collection"
  return(result)
}
validateResultCollection <- function(result) {
  if (!"result_collection" %in% class(result)) {
    cli::cli_abort(
      "A result collection must be created with te function `resultCollection`."
    )
  }
  return(result)
}

#' @export
print.result_collection <- function(x, ...) {
  len <- length(x)
  types <- lapply(x, function(result) {
    result |>
      dplyr::select("result_type") |>
      dplyr::distinct() |>
      dplyr::pull()
  })
  tib <- dplyr::tibble(name = names(x), type = unname(unlist(types))) |>
    dplyr::group_by(.data$type) |>
    dplyr::summarise(
      count = dplyr::n(),
      names = paste0(.data$name, collapse = ", "),
      .groups = "drop"
    )
  mes <- paste0(cli::style_bold("Results collection"), " [", element(len), "]")
  for (k in seq_len(nrow(tib))) {
    mes <- c(
      mes,
      "*" = paste0(
        cli::style_bold(tib$type[k]), " [", element(tib$count[k]), "]: ",
        tib$names[k]
      )
    )
  }
  cli::cli_bullets(mes)
}

element <- function(n) {
  if (n == 1) {
    return("1 element")
  } else {
    return(paste0(n, " elements"))
  }
}
