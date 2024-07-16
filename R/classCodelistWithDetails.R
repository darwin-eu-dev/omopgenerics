# Copyright 2024 DARWIN EU (C)
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


#' 'codelist' object constructor
#'
#' @param x A named list where each element contains a tibble with the column
#' concept_id
#'
#' @return A codelist object.
#'
#' @export
#'
newCodelistWithDetails <- function(x) {

  #constructor
  x <- constructCodelistWithDetails(x)

  # validate
  x <- validateCodelistWithDetails(x)

  return(x)
}

constructCodelistWithDetails <- function(x) {
  x |> addClass("codelist_with_details")
}

validateCodelistWithDetails <- function(x) {

  assertList(x, named = TRUE, class = c("data.frame", "tbl_df"))

    for (nm in names(x)) {
      if(isFALSE(any("concept_id" %in% colnames(x[[nm]])))){
        cli::cli_abort("`{nm}` column concept_id not found")
      }

      if (any(is.na(unique(x[[nm]]$concept_id)))) {
        cli::cli_abort("`{nm}` must not contain NA in concept_id field.")
      }
    }

  return(x)
}


#' Print a codelist with details
#'
#' @param x A codelist with details
#' @param ...  Included for compatibility with generic. Not used.
#'
#' @return  Invisibly returns the input
#' @export
#'
#' @examples
#' codes <- list("disease X" = dplyr::tibble(concept_id = c(1, 2, 3),
#'                                           other= c("a", "b", "c")))
#' codes <- newCodelistWithDetails(codes)
#' print(codes)
#'
print.codelist_with_details <- function(x, ...) {

  cli::cli_h1("{length(x)} codelist{?s} with details")
  cli::cat_line("")
  if(length(x) <= 6){
    for(i in seq_along(x)){
      cli::cat_line(paste0("- ", names(x)[i], " (", length(x[[i]]$concept_id), " codes)"))
    }
  } else {
    for(i in seq_along(x[1:6])){
      cli::cat_line(paste0("- ", names(x[1:6])[i], " (", length(x[[i]]$concept_id), " codes)"))
    }
    cli::cat_line(paste0("along with ", length(x)-6, " more codelists"))
  }
  invisible(x)
}

#' Empty `codelist` object.
#'
#' @return An empty codelist object.
#'
#' @export
#'
#' @examples
#' emptyCodelistWithDetails()
#'
emptyCodelistWithDetails <- function() {
  newCodelistWithDetails(list())
}
