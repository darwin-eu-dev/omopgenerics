# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
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
#'
#' setTemporary(intermediateAsTemp = TRUE, cohortAsTemp = FALSE)
#' }
#'
setTemporary <- function(intermediateAsTemp = TRUE,
                         cohortAsTemp = FALSE) {
  # check inputs
  #checkInput(
  #  intermediateAsTemp = intermediateAsTemp, cohortAsTemp = cohortAsTemp
  #)

  # set options
  options("intermediate_as_temp" = intermediateAsTemp)
  options("cohort_as_temp" = cohortAsTemp)

  # return
  return(invisible(NULL))
}
