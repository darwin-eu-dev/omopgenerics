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

addResultId <- function(x, resultId) {
  if (!is.null(resultId)) {
    x <- x |>
      dplyr::mutate("result_id" = .env$resultId) |>
      dplyr::relocate("result_id")
  }
  return(x)
}
saveFile <- function(x, path, namePrefix, resultId, nam) {
  readr::write_csv(
    x = x, file = file.path(path, fileName(x, path, namePrefix, resultId, nam))
  )
}
fileName <- function(x, path, namePrefix, resultId, nam) {
  n <- nchar(namePrefix)
  if (n > 0 & substr(namePrefix, n, n) != "_") {
    namePrefix <- paste0(namePrefix, "_")
  }
  if (!is.null(resultId)) {
    resultId <- paste0("_", resultId)
  }
  paste0(
    namePrefix, nam, "_", paste0(unique(x$cdm_name), collapse = "_"), resultId,
    ".csv"
  )
}
getVocabularyVersion <- function(x) {
  vocabVersion <- NULL
  if ("vocabulary_version" %in% colnames(x[["vocabulary"]])) {
    vocabVersion <- x[["vocabulary"]] |>
      dplyr::filter(.data$vocabulary_id == "None") |>
      dplyr::pull(.data$vocabulary_version)
  }
  if (length(vocabVersion) == 0) {
    vocabVersion <- NA_character_
  }
  return(vocabVersion)
}
