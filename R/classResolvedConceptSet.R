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

#' `resolved_concept_set` objects constructor.
#'
#' @param codelist List with each item containing either a vector of concept IDs
#' or a tibble with a column called concept_id.
#'
#' @return A resolved_concept_set object
#'
#' @export
#'
resolvedConceptSet <- function(cohortTable) {
  UseMethod("resolvedConceptSet")
}

