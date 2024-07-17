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

#' Import a concept set expression.
#'
#' @param path Path to where files will be created.
#' @param type Type of files to export. Currently only "json" is supported.
#'
#' @return A concept set expression
#' @export
importConceptSetExpression <- function(path, type){

  rlang::check_installed("jsonlite")
  if(type != "json"){
    cli::cli_abort(c("x" = "Only json files are currently supported."))
  }
  if(!dir.exists(path)){
    cli::cli_abort(c("x" = "Given path does not exist"))
  }

  jsonFiles <- list.files(path,
                          pattern = "\\.json$",
                          full.names = TRUE)
  jsonNames <- list.files(path,
                          pattern = "\\.json$",
                          full.names = FALSE)
  jsonNames <- stringr::str_remove_all(jsonNames, ".json")

  tidyJson <- NULL
  for(i in seq_along(jsonFiles)){
    workingJson <- jsonlite::fromJSON(jsonFiles[[i]])
    tidyJson[[i]] <-  dplyr::tibble(
                  concept_id = workingJson$items$concept$CONCEPT_ID,
                  excluded = workingJson$items$isExcluded,
                  descendants = workingJson$items$includeDescendants,
                  mapped = workingJson$items$includeMapped) |>

      dplyr::mutate(name = jsonNames[[i]])

    if(nrow(tidyJson[[i]]) == 0){
      cli::cli_warn("{jsonNames[[i]]}.json could not be parsed as a concept set expression")
    }

  }
  tidyJson <- dplyr::bind_rows(tidyJson)

  cse <- split(tidyJson, f = tidyJson$name)
  for(i in seq_along(cse)){
    cse[[i]] <- cse[[i]] |>
      dplyr::select(!"name")
  }

    newConceptSetExpression(cse)

}
