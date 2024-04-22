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

#' Validate a character vector to ensure it is snake case, it will be converted
#' to snake case otherwise.
#'
#' @param string A character vector to validate if it is snake_case.
#' @param type Can be either, "message" or "silent".
#' @param call The corresponding function call is retrieved and mentioned in
#' error messages as the source of the error.
#'
#' @export
#'
#' @return A snake_case character
#'
validateSnakeCase <- function(string,
                              type = "message",
                              call = parent.frame()) {
  # check input
  assertCharacter(x = string, call = call)
  assertChoice(x = type, choices = c("message", "silent"), length = 1, call = call)

  # validation
  newString <- toSnakeCase(string)
  id <- which(string != newString)
  if (length(id) > 0) {
    x <- character()
    for (i in id) {
      x <- c(x, "*" = paste0(string[i], " was changed to ", newString[i]))
    }
    report(x = x, type = type, call = call)
  }

  # return
  return(newString)
}
