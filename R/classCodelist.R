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


#' 'codelist' object constructor
#'
#' @param x a named list where each element contains a vector of concept
#' IDs
#'
#' @return A codelist
#' @export
#'
#' @examples
codelist <- function(x) {

  #constructor
  x <- newCodelist(x)

  # validate
  x <- validateCodelist(x)

  return(x)
}

newCodelist <- function(x) {

  class(x) <- c("codelist", class(x)[which(class(x) != "codelist")])

  return(x)
}

validateCodelist <- function(x) {

  assertList(x, named = TRUE, class = c("numeric", "integer"))
  checkNA(x, names(x))

  return(x)
}

#' @export
print.codelist <- function(x, ...) {
  cli::cli_h1("{length(x)} codelist{?s}")
  cli::cat_line("")
  if(length(x) <= 6){
    for(i in seq_along(x)){
      cli::cat_line(paste0("- ", names(x)[i], " (", length(x[[i]]), " codes)"))
    }
  } else {
    for(i in seq_along(x[1:10])){
      cli::cat_line(paste0("- ", names(x[1:10])[i], " (", length(x[[i]]), " codes)"))
    }
    cli::cat_line(paste0("along with ", length(x)-10, " more codelists"))
  }
  invisible(x)

}
