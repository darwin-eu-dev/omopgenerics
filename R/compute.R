# Copyright 2023 DARWIN EU (C)
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

#' Store results in a table.
#'
#' @param x Table in the cdm.
#' @param name Name to store the table with.
#' @param temporary Whether to store table temporarily (TRUE) or permanent
#' (FALSE).
#' @param overwrite Whether to overwrite previously existing table with name
#' same.
#' @param ... For compatibility (not used).
#'
#' @return Reference to a table in the cdm
#'
#' @export
#' @importFrom dplyr compute
compute.cdm_table <- function(x,
                              name = uniqueTableName(),
                              temporary = TRUE,
                              overwrite = TRUE,
                              ...) {
  src <- tableSource(x)
  cl <- class(src)[class(src) != "cdm_source"]
  cx <- class(x)
  res <- x |>
    keepClass() |>
    addClass(cl) |>
    dplyr::compute(name = name, temporary = temporary, overwrite = overwrite)
  if (temporary) name <- NA_character_
  res <- res |>
    removeClass(cl) |>
    newCdmTable(src = src, name = name) |>
    restoreClass(cx) |>
    restoreAttributes(keepAttributes(x, cx))
  return(res)
}

#' @export
compute.local_cdm <- function(x, ...) {
  return(x)
}

#' Create a unique table name
#'
#' @param prefix Prefix for the table names.
#'
#' @return A string that can be used as a dbplyr temp table name
#' @export
#'
#' @examples
#' library(omopgenerics)
#' uniqueTableName()
uniqueTableName <- function(prefix = "") {
  assertCharacter(x = prefix, length = 1)
  i <- getOption("og_table_name", 0) + 1
  options(og_table_name = i)
  value <- paste0(sprintf("og_%03i", i), "_", round(as.numeric(Sys.time())))
  paste0(prefix, value)
}

#' Create a temporary prefix for tables, that contains a unique prefix that
#' starts with tmp.
#'
#' @return A temporary prefix.
#' @export
#'
#' @examples
#' library(omopgenerics)
#' tmpPrefix()
tmpPrefix <- function() {
  i <- getOption("tmp_prefix_number", 0) + 1
  options(tmp_prefix_number = i)
  sprintf("tmp_%03i_", i)
}

