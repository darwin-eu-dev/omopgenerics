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

#' Store results in a table.
#'
#' @param x Table in the cdm.
#' @param name Name to store the table with.
#' @param temporary Whether to store table temporarily (TRUE) or permanent
#' (FALSE).
#' @param overwrite Whether to overwrite previosly existing table with name
#' same.
#' @param ... For compatibility (not used).
#'
#' @export
#' @importFrom dplyr compute
compute.cdm_table <- function(x, name, temporary = TRUE, overwrite = TRUE, ...) {
  cdm <- attr(x, "cdm_reference")
  if (is.null(attr(x, "cdm_reference"))) {
    cli::cli_abort("x does not com from a cdm object.")
  }
  src <- getCdmSource(x)
  x <- addClass(x, class(src))
  x <- dplyr::compute(x = x, name = name, temporary = temporary, overwrite = overwrite)
  x <- removeClass(x, class(src))
  return(x)
}

#' @export
compute.local_cdm <- function(x, name, temporary, overwrite, ...) {
  attr(x, "tbl_name") <- name
  return(x)
}
