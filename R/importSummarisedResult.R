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

#' Import a set of summarised results.
#'
#' @param path Path to directory with CSV files containing summarised results or
#' to a specific CSV file with a summarised result.
#'
#'
#' @return A summarised result
#' @export
#'
importSummarisedResult <- function(path){
  rlang::check_installed("readr")

  assertCharacter(path, length = 1,
                  msg = "Only a single path can be specified")

  if(stringr::str_sub(path, -4, -1) == ".csv"){
    isDir <- FALSE
    if(!file.exists(path)){
      cli::cli_abort(c("x" = "Given file does not exist"))
    }
  } else {
    isDir <- TRUE
    if(!dir.exists(path)){
      cli::cli_abort(c("x" = "Given path does not exist"))
    }
  }


  if(isDir){
  csvFiles <- list.files(path,
                          pattern = "\\.csv$",
                          full.names = TRUE)
  csvFileNames <- list.files(path,
                         pattern = "\\.csv$",
                         full.names = FALSE)
  } else {
    csvFiles <- path
    csvFileNames <- basename(path)
  }

  allResults <- NULL
  for(i in seq_along(csvFiles)){
    allResults[[i]] <- readr::read_csv(csvFiles[[i]],
                                       col_types = c(.default = "c",
                                                     result_id = "integer"),
                                       show_col_types = FALSE)
    if(isFALSE(setequal(sort(colnames(allResults[[i]])), sort(resultColumns())))){
     cli::cli_abort("{csvFileNames[[i]]} does not have summarised result columns")
    }

    allResults[[i]] <- newSummarisedResult(allResults[[i]])
  }
  allResults <- bind(allResults)

  allResults
}
