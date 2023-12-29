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

# library(dplyr)
# funs <- lsf.str("package:dplyr")
# funsMethods <- character()
# for (fun in funs) {
#   try(
#     if (isS3stdGeneric(eval(parse(text = fun)))) {
#       funsMethods <- c(funsMethods, fun)
#     },
#     silent = TRUE
#   )
# }
# funsMethods <- c(
#   "anti_join", "collapse", "count", "cross_join", "full_join", "group_by",
#   "inner_join", "intersect", "left_join", "nest_join",
#   "right_join", "rowwise", "semi_join", "tally", "ungroup", "union",
#   "union_all", "summarise"
# )
# x <- character()
# for (fun in funsMethods) {
#   x <- c(
#     x,
#     "#' @export",
#     paste0("#' @importFrom dplyr ", fun),
#     paste0(fun, ".cdm_table <- function(x, ...) {"),
#     "  x <- keepClass(x)",
#     paste0("  res <- dplyr::", fun, "(x, ...)"),
#     "  res <- restoreClass(res)",
#     "  res <- restoreAttributes(res, keepAttributes(x))",
#     "  return(res)",
#     "}",
#     ""
#   )
# }
# cat(x, sep = "\n")

#' @export
#' @importFrom dplyr anti_join
anti_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::anti_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr count
count.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::count(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr cross_join
cross_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::cross_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr full_join
full_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::full_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr group_by
group_by.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  .data <- keepClass(.data)
  res <- dplyr::group_by(.data = .data, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(.data, cl))
  return(res)
}

#' @export
#' @importFrom dplyr inner_join
inner_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::inner_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr intersect
intersect.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::intersect(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr left_join
left_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::left_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr nest_join
nest_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::nest_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr right_join
right_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::right_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr rowwise
rowwise.cdm_table <- function(data, ...) {
  cl <- class(.data)
  data <- keepClass(data)
  res <- dplyr::rowwise(data = data, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(data, cl))
  return(res)
}

#' @export
#' @importFrom dplyr semi_join
semi_join.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::semi_join(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr tally
tally.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::tally(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr ungroup
ungroup.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::ungroup(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr union
union.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::union(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr union_all
union_all.cdm_table <- function(x, ...) {
  cl <- class(x)
  x <- keepClass(x)
  res <- dplyr::union_all(x, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(x, cl))
  return(res)
}

#' @export
#' @importFrom dplyr summarise
summarise.cdm_table <- function(.data, ...) {
  cl <- class(.data)
  .data <- keepClass(.data)
  res <- dplyr::summarise(.data = .data, ...)
  res <- restoreClass(res, cl)
  res <- restoreAttributes(res, keepAttributes(.data, cl))
  return(res)
}

keepAttributes <- function(x, cl) {
  xx <- list(
    tbl_source = attr(x, "tbl_source"),
    tbl_name = attr(x, "tbl_name"),
    cdm_reference = attr(x, "cdm_reference")
  )
  if ("generated_cohort_set" %in% cl) {
    xx[["cohort_set"]] <- attr(x, "cohort_set")
    xx[["cohort_attrition"]] <- attr(x, "cohort_attrition")
  }
  return(xx)
}
keepClass <- function(x) {
  x |>
    removeClass(c(
      "cdm_table", "omop_table", "achilles_table", "generated_cohort_set"
    ))
}
restoreAttributes <- function(x, at) {
  for (nm in names(at)) {
    if (!nm %in% names(attributes(x))) {
      attr(x, nm) <- at[[nm]]
    }
  }
  return(x)
}
restoreClass <- function(x, cl) {
  x <- addClass(x, "cdm_table")
  if ("generated_cohort_set" %in% cl &
      "cohort_definition_id" %in% colnames(x)) {
    x <- addClass(x, "generated_cohort_set")
  }
  return(x)
}
