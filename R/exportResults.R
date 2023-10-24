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

#' Export list of results
#'
#' @param ... Named list of results.
#' @param path Path to save the results.
#' @param resultsStem Stem to save the results csv files. Name of the zip file
#' if exported.
#' @param zip Whether or not to create a zip file with all the results.
#'
#' @export
#'
exportResults <- function(...,
                          path = here::here(),
                          resultsStem = "results",
                          studyId = NULL,
                          zip = TRUE) {
  # initial checks
  elements <- list(...)
  #checkInput(
  #  elements = elements, path = path, resultsStem = resultsStem, zip = zip,
  #  studyId = studyId
  #)

  # correct names
  names(elements) <- paste0(resultsStem, names(elements), ".csv")

  # export
  for (k in seq_along(elements)) {
    element <- export(elements[[k]]) %>%
      dplyr::mutate("study_id" = .env$studyId) %>%
      dplyr::relocate("study_id")
    readr::write_csv(x = element, file = paste0(path, "/", names(elements)[k]))
  }

  # zip if needed
  if (zip == TRUE) {
    zip::zipr(
      zipfile = file.path(here::here(paste0(resultsStem, ".zip"))),
      files = names(elements)
    )
  }

  # return
  return(invisible(NULL))
}
