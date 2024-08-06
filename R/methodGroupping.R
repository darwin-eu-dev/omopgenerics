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

#' Get groupping from an object.
#'
#' @param x Object
#'
#' @return A table with the groupping of the object.
#'
#' @export
groupping <- function(x) {
  UseMethod("groupping")
}

#' Get groupping from a omop_result object.
#'
#' @param x A omop_result object.
#'
#' @return A table with the groupping.
#'
#' @export
#'
groupping.omop_result <- function(x) {
  attr(x, "groupping")
}

