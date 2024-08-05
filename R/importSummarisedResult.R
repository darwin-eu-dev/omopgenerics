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
#' @param recursive If TRUE and path is a directory, search for files will
#' recurse into directories
#'
#'
#' @return A summarised result
#' @export
#'
importSummarisedResult <- function(path,
                                   recursive = FALSE){
  rlang::check_installed("readr")

  result <- list()
  for(i in seq_along(path)){
  result[[i]] <- importSummarisedResultFromPath(path = path[i],
                                 recursive = recursive)
  }
  result <- bind(result) |>
    newSummarisedResult() |>
    dplyr::arrange(.data$cdm_name,
                   .data$result_id)

  result

}

importSummarisedResultFromPath <- function(path,
                                           recursive){

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
                           recursive = recursive,
                           pattern = "\\.csv$",
                           full.names = TRUE)
    csvFileNames <- list.files(path,
                               recursive = recursive,
                               pattern = "\\.csv$",
                               full.names = FALSE)
  } else {
    csvFiles <- path
    csvFileNames <- basename(path)
  }

  if(length(csvFiles) == 0){
    cli::cli_warn("No csv files found in directory. Returning an empty summarised result.")
    return(omopgenerics::emptySummarisedResult())
    }

  allResults <- list()
  allResults[["empty"]] <- omopgenerics::emptySummarisedResult()
  for(i in seq_along(csvFiles)){
    cli::cli_inform("Reading {csvFiles[[i]]}")
    allResults[[i]] <- readr::read_csv(csvFiles[[i]],
                                       col_types = c(.default = "c",
                                                     result_id = "integer"),
                                       show_col_types = FALSE)


    if(isFALSE(setequal(sort(colnames(allResults[[i]])), sort(resultColumns())))){
     cli::cli_warn("{csvFileNames[[i]]} does not have summarised result columns and so was omitted")
      allResults[[i]] <- omopgenerics::emptySummarisedResult()
      } else {
      allResults[[i]] <- newSummarisedResult(allResults[[i]])
    }

  }
  allResults <- bind(allResults)

  allResults

}
