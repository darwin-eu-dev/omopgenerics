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
#' @param x a named list where each element contains either a vector of concept
#' IDs or a tibble with a column concept_id that contains concept IDs
#' representing the codelist
#' @param name
#'
#' @return
#' @export
#'
#' @examples
codelist <- function(x, name = "codelist") {

  #constructer
  x <- newCodelist(x, name)

  # validate
  x <- validateCodelist(x)

  return(x)
}

newCodelist <- function(x, name) {

  class(x) <- c("codelist", class(x))
  attr(x, "codelist") <- name

  return(x)
}

validateCodelist <- function(x) {


  # must be a named list

  # names in the list must be snake case with a maximum length of ??

  # either a vector of concept ids (numeric) or a tibble with a column that
  # is concept ids

  # none can be NA


  return(x)
}
