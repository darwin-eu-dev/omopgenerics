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

#' #' Extract a table from a cdm reference.
#' #'
#' #' @param table
#' #'
#' #' @return A table from the cdm reference
#' #'
#' #' @export
#' #'
#' cdmTable <- function(table) {
#'   addClass(table) <- "cdm_table"
#'   if ("generated_cohort_set" %in% class(table)) {
#'     addClass(attr(table, "cohort_set")) <- "cdm_table"
#'     addClass(attr(table, "cohort_attrition")) <- "cdm_table"
#'   }
#'   return(table)
#' }
#'
#' #' @importFrom dplyr compute
#' #' @export
#' compute.cdm_table <- function(x, name) {
#'   computeTable(x = x, name = name)
#' }
#'
#' #' @export
#' #' @importFrom dplyr arrange
#' arrange.cdm_table <- function(.data, ...) {
#'   if ("tbl_lazy" %in% class(.data)) {
#'     x <- dbplyr::window_order(.data, ...)
#'   } else {
#'     removeClass(.data) <- "cdm_table"
#'     x <- dplyr::arrange(.data, ...)
#'     addClass(.data) <- "cdm_table"
#'   }
#'   return(x)
#' }
