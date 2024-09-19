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

#' Provide all combinations of strata levels.
#'
#' @param levels Vector of all strata levels to combine.
#'
#' @return A vector of all combinations of strata.
#' @export
combineStrata <- function(levels) {
  # Checks
  assertCharacter(levels, null = TRUE, na = TRUE)

  # empty list if NULL or character()
  if(length(levels) == 0) return(list())

  # Apply combn function to all lengths of combinations
  result <- seq_along(levels) |>
    purrr::map(\(x) utils::combn(x = levels, m = x, simplify = FALSE)) |>
    unlist(recursive = FALSE)

  return(result)
}
