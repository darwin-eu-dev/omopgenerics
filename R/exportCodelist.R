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

#' Export a codelist object.
#'
#' @param x A codelist
#' @param path Path to where files will be created.
#' @param type Type of files to export. Currently only "json" is supported.
#'
#' @return Files with codelists
#' @export
#'
exportCodelist <- function(x, path, type = "json"){

  rlang::check_installed("jsonlite")
  if(type != "json"){
    cli::cli_abort(c("x" = "Only json files are currently supported."))
  }
  if(!dir.exists(path)){
    cli::cli_abort(c("x" = "Given path does not exist"))
  }

  x <- validateCodelist(x)

  for (name in names(x)) {
    items <- list()
    tibble_df <- dplyr::tibble("concept_id" = x[[name]])
    for (i in 1:nrow(tibble_df)) {
      item <- list(
        concept = list(
          CONCEPT_ID = tibble_df$concept_id[i],
          CONCEPT_NAME = "",
          STANDARD_CONCEPT = "",
          STANDARD_CONCEPT_CAPTION = "",
          INVALID_REASON = "",
          INVALID_REASON_CAPTION = "",
          CONCEPT_CODE = "",
          DOMAIN_ID = "",
          VOCABULARY_ID = "",
          CONCEPT_CLASS_ID = ""
        ),
        isExcluded = FALSE,
        includeDescendants = FALSE,
        includeMapped = FALSE
      )
      items <- append(items, list(item))
    }
    output_list <- list(items = items)
    jsonlite::write_json(output_list,
                         path = paste0(path, "/", name, ".json"),
                         pretty = TRUE, auto_unbox = TRUE)
  }

  return(invisible(NULL))

}
