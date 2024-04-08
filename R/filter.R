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
filter.summarised_result <- function(.data, ...) {
  if ("result_id" %in% colnames(.data)) {
    cols <- colnames(settings(.data))
    cols <- cols[!cols %in% colnames(.data)]
    .data <- .data |> addSettings()
  } else {
    cols <- character()
  }
  cl <- class(.data)
  res <- keepClass(.data)
  res <- res |>
    dplyr::filter(...) |>
    dplyr::select(!dplyr::all_of(cols))
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(.data, cl))
  return(res)
}
