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

#' @export
#' @importFrom dplyr arrange
arrange.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  .data <- keepClass(.data)
  if ("tbl_lazy" %in% cl) {
    res <- dbplyr::window_order(.data, ...)
  } else {
    res <- dplyr::arrange(.data, ...)
  }
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(.data, cl))
  return(res)
}
