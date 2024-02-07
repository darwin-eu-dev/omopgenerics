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

#' Export list of package results
#'
#' @param resultList Named list with results from a darwin package
#' @param zipName name to give zip folder
#' @param outputFolder directory to save zip folder containing results as a set
#' of CSV files
#'
#' @return zip folder of results saved in the outputFolder
#' @export
#'
#' @examples
#' \donttest{
#' exportResults(
#'   resultList = list("mtcars" = mtcars),
#'   zipName = "test",
#'   outputFolder = tempdir()
#' )
#' }
exportResults <- function(resultList,
                          zipName,
                          outputFolder) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertList(resultList, add = errorMessage)
  checkmate::assertNamed(resultList, add = errorMessage)
  checkmate::assertCharacter(zipName, add = errorMessage)
  checkmate::assertDirectoryExists(outputFolder, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  tempDir <- zipName
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }

  # write results to disk
  for (i in seq_along(resultList)) {
    workingResult <- resultList[[i]]
    workingName <- names(resultList)[[i]]
    if (is.null(workingName)) {
      workingName <- paste0("result_", i)
    }
    utils::write.csv(workingResult,
                     file = file.path(
                       tempDir,
                       paste0(
                         unique(workingResult$cdm_name), "_",
                         workingName, "_",
                         format(Sys.Date(), format = "%Y_%m_%d"),
                         ".csv"
                       )
                     ),
                     row.names = FALSE
    )
  }
  zip::zip(
    zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
    files = list.files(tempDir, full.names = TRUE)
  )
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }

  invisible(resultList)
}
