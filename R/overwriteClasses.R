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

keepAttributes <- function(x) {
  list(
    cdm_reference = attr(x, "cdm_reference"),
    tbl_name = attr(x, "tbl_name")
  )
}
keepClass <- function(x) {
  x <- removeClass(x, "cdm_table")
  return(x)
}
restoreAttributes <- function(x, at) {
  for (nm in names(at)) {
    if (!nm %in% names(attributes(x))) {
      attr(x, nm) <- at[[nm]]
    }
  }
  return(x)
}
restoreClass <- function(x) {
  x <- addClass(x, "cdm_table")
  return(x)
}

#' @export
#' @importFrom dplyr group_by
group_by.cdm_table <- function(.data, ...) {
  .data <- keepClass(.data)
  x <- dplyr::group_by(.data, ...)
  x <- restoreClass(x)
  x <- restoreAttributes(x, keepAttributes(.data))
  return(x)
}

#' @export
#' @importFrom dplyr summarise
summarise.cdm_table <- function(.data, ...) {
  .data <- keepClass(.data)
  x <- dplyr::summarise(.data, ...)
  x <- restoreClass(x)
  x <- restoreAttributes(x, keepAttributes(.data))
  return(x)
}

# add_count
# anti_join
# arrange
# as.data.frame
# auto_copy
# count
# cross_join
# distinct
# dplyr_col_modify
# dplyr_reconstruct
# dplyr_row_slice
# filter
# full_join
# group_data
# inner_join
# intersect
# left_join
# mutate
# nest_by
# nest_join
# right_join
# rows_append
# rows_delete
# rows_insert
# rows_patch
# rows_update
# rows_upsert
# rowwise
# sample_n
# select
# semi_join
# slice
# slice_head
# slice_max
# slice_min
# slice_sample
# slice_tail
# symdiff
# tally
# tbl
# transmute
# ungroup
# union
# union_all
