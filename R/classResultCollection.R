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
newResultsCollection <- function(...) {
  results <- list(...)
  if (length(results) == 1 & is.null(names(results))) {
    results <- unlist(results)
  }
  # initial input check
  checkInput(results = results)

  class(results) <- "results_collection"

 return(results)
}

#' @export
print.results_collection <- function(x, ...) {
  len <- length(x)
  types <- lapply(x, function(result) {
    result %>%
      dplyr::select("result_type") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  })
  tib <- dplyr::tibble(name = names(x), type = unname(unlist(types))) %>%
    dplyr::group_by(.data$type) %>%
    dplyr::summarise(
      count = dplyr::n(),
      names = paste0(.data$name, collapse = ", "),
      .groups = "drop"
    )
  mes <- paste0(cli::style_bold("Results collection"), " [", element(len), "]")
  for (t in tib$type) {
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
