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


#' 'codelist' object constructor
#'
#' @param x A named list where each element contains a vector of concept IDs.
#'
#' @return A codelist object.
#'
#' @export
#'
newCodelist <- function(x) {

  #constructor
  x <- constructCodelist(x)

  # validate
  x <- validateCodelist(x)

  return(x)
}

constructCodelist <- function(x) {
  x |> addClass("codelist")
}

validateCodelist <- function(codelist, call = parent.frame()) {

  assertList(codelist, named = TRUE,
             class = c("numeric", "integer", "integer64"), call = call)

  for (nm in names(codelist)) {
    if (any(is.na(unique(codelist[[nm]])))) {
      cli::cli_abort("`{nm}` must not contain NA.", call = call)
    }
  }

  # alphabetical order
  if(length(codelist) > 0 ){
  codelist <- codelist[order(names(codelist))] |>
    addClass("codelist")
  }

  return(codelist)
}


#' Print a codelist
#'
#' @param x A codelist
#' @param ...  Included for compatibility with generic. Not used.
#'
#' @return  Invisibly returns the input
#' @export
#'
#' @examples
#' codes <- list("disease X" = c(1, 2, 3), "disease Y" = c(4, 5))
#' codes <- newCodelist(codes)
#' print(codes)
#'
print.codelist <- function(x, ...) {
  cli::cli_h1("{length(x)} codelist{?s}")
  cli::cat_line("")
  if(length(x) <= 6){
    for(i in seq_along(x)){
      cli::cat_line(paste0("- ", names(x)[i], " (", length(x[[i]]), " codes)"))
    }
  } else {
    for(i in seq_along(x[1:6])){
      cli::cat_line(paste0("- ", names(x[1:6])[i], " (", length(x[[i]]), " codes)"))
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
#' emptyCodelist()
#'
emptyCodelist <- function() {
  newCodelist(list())
}
