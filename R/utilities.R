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

addClass <- function(x, value) {
  print("addclass")
  if (any(value %in% class(x))) x <- removeClass(x, value)
  base::class(x) <- c(value, base::class(x))
  return(x)
}
removeClass <- function(x, value) {
  print("removeclass")
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
getCdmSource <- function(x) {
  print("get_source")
  if ("cdm_reference" %in% class(x)) {
    cdm <- x
  } else {
    cdm <- attr(x, "cdm_reference")
  }
  if (is.null(cdm)) {
    return(NULL)
  } else {
    return(attr(cdm, "cdm_source"))
  }
}
