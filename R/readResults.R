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

#' Read a set of files and create a `results_collection`.
#'
#' @param path Path to a file or folder with results
#'
#' @return A `result_collection` object.
#'
#' @export
#'
readResults <- function(path) {
  checkInput(path = path)
  results <- list.files(path = path, full.names = TRUE)
  results <- results[tools::file_ext(results) == ".csv"]
  collection <- list()
  for (file in results) {
    collection[[basename(file)]] <- readr::read_csv(file)
  }
  return(newResultsCollection(collection))
}
