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

#' 'omop_result' object constructor
#'
#' @param x Table.
#' @param settings Settings for the omop_result object. It can also refer to
#' the columns to build the settings table.
#' @param groupping Groupping for the omop_result object. It can also refer to
#' the columns to build the groupping table.
#'
#' @return A `omop_result` object
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
newOmopResult <- function(x,
                          settings = attr(x, "settings"),
                          groupping = attr(x, "groupping")) {
  # inital input check
  assertClass(x = x, class = "data.frame")
  if (is.character(settings)) {
    x <- buildAttribute(x, "settings", settings)
    settings <- attr(x, "settings")
  } else if (is.null(settings)) {
    x <- getAttribute(x, "settings")
    settings <- attr(x, "settings")
  }
  if (is.character(groupping)) {
    x <- buildAttribute(x, "groupping", groupping)
    groupping <- attr(x, "groupping")
  } else if (is.null(groupping)) {
    x <- getAttribute(x, "groupping")
    groupping <- attr(x, "groupping")
  }
  assertTable(settings, columns = "settings_id")
  assertTable(groupping, columns = "groupping_id")

  # constructor
  x <- constructOmopResult(x, settings, groupping)

  # validate
  x <- validateSummariseResult(x)

  return(x)
}

buildAttribute <- function(x, id, cols, call = parent.frame()) {
  name <- paste0(id, "_id")
  if (any(is.na(cols))) {
    cli::cli_abort("{.var {id}} can not contain NA.", call = call)
  }
  if (length(cols) != length(unique(cols))) {
    cli::cli_abort("{.var {id}} must be unique.", call = call)
  }
  notPresent <- cols[!cols %in% colnames(x)]
  if (length(notPresent) > 0) {
    cli::cli_abort("{.var {notPresent}} not present in data.", call = call)
  }
  if (length(cols) == 0) {
    set <- dplyr::tibble(!!name := 1L)
    x <- x |>
      dplyr::mutate(!!name := 1L)
  } else {
    set <- x |>
      dplyr::select(dplyr::all_of(cols)) |>
      dplyr::distinct() |>
      dplyr::mutate(!!name := as.integer(dplyr::row_number()))
    x <- x |>
      dplyr::inner_join(set, by = cols) |>
      dplyr::select(!dplyr::all_of(cols))
  }
  attr(x, name) <- set
  return(x)
}
getAttribute <- function(x, id, call = parent.frame()) {
  cols <- c(
    paste0(id, "_id"), "estimate_name", "estimate_type", "estimate_value")
  names(cols) <- stringr::str_replace(cols, "estimate", id)
  set <- x |>
    dplyr::filter(.data$variable_name == id) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::distinct()
  return(set)
}
constructSummarisedResult <- function(x, settings, group, call = parent.frame()) {
  x <- x |>
    dplyr::as_tibble() |>
    dplyr::distinct()

  # try to build settings
  # settings == NULL -> get from lines
  # settings != NULL -> error if settings is in variable
  # is character -> get from columns
  # if is table thats settings

  set <- getAttribute(x, "settings")
  if (is.character(settings)) {
    assertTable(x, columns = settings, call = call)
    set2 <- x |>
      dplyr::select(dplyr::all_of(settings)) |>
      dplyr::distinct() |>
      dplyr::mutate()
  } else {
    assertTable(settings, columns = c(
      "settings_id", "settings_name", "settings_type", "settings_value"),
      null = TRUE, call = call)
  }

  set <- set |>
    dplyr::as_tibble() |>
    pivotLong(id = "settings")

  x <- x |> dplyr::select(!dplyr::all_of(settingsCols))
  extraSets <- x |>
    dplyr::filter(.data$variable_name == "settings") |>
    dplyr::select(
      "result_id", "estimate_name", "estimate_type", "estimate_value"
    )
  if (nrow(extraSets) > 0) {
    errorRows <- extraSets |>
      dplyr::group_by(.data$result_id, .data$estimate_name) |>
      dplyr::tally(name = "n") |>
      dplyr::filter(.data$n > 1) |>
      dplyr::pull("estimate_name") |>
      unique()
    if (length(errorRows) > 0) {
      cli::cli_alert_warning("The following settings have duplicate values for same result_id: {paste0(errorRows, collapse = ', ')}.")
    }
    extraSets <- extraSets |>
      tidyr::pivot_wider(
        names_from = c("estimate_type", "estimate_name"),
        values_from = "estimate_value"
      ) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::starts_with("numeric|proportion|percentage"),
          .fns = as.numeric
        ),
        dplyr::across(
          .cols = dplyr::starts_with("integer"),
          .fns = as.integer
        ),
        dplyr::across(
          .cols = dplyr::starts_with("date"),
          .fns = as.Date
        ),
        dplyr::across(
          .cols = dplyr::starts_with("logical"),
          .fns = as.logical
        )
      ) |>
      dplyr::rename_with(.fn = ~ sub("^[^_]*_", "", .x), .cols = !"result_id")
    set <- set |> joinSet(extraSets)
    x <- x |> dplyr::filter(.data$variable_name != "settings")
  }

  if (is.null(set)) {
    set <- x |> dplyr::select("result_id") |> dplyr::distinct()
  }

  x <- structure(
    .Data = x,
    class = unique(c("summarised_result", "omop_result", class(x))),
    settings = set |>
      dplyr::mutate("result_id" = as.integer(.data$result_id)) |>
      dplyr::relocate("result_id") |>
      dplyr::arrange(.data$result_id)
  )

  return(x)
}
validateSummariseResult <- function(x) {
  if (!"result_id" %in% colnames(x)) {
    x <- x |> dplyr::mutate("result_id" = as.integer(1))
    warnResult <- TRUE
  } else {
    warnResult <- FALSE
  }

  # compulsory columns
  x <- checkColumns(x = x, "summarised_result")
  if (warnResult) {
    cli::cli_warn(c(
      "!" = "`result_id` column is missing, please add it as it is a compulsory
      column."
    ))
  }

  # all columns should be character
  x <- checkColumnsFormat(x = x, "summarised_result")

  # Cannot contain NA columns
  checkNA(x = x, "summarised_result")

  # columPairs
  validateNameLevel(
    x = x, nameColumn = "group_name", levelColumn = "group_level", warn = TRUE
  )
  validateNameLevel(
    x = x, nameColumn = "strata_name", levelColumn = "strata_level", warn = TRUE
  )
  validateNameLevel(
    x = x, nameColumn = "additional_name", levelColumn = "additional_level",
    warn = TRUE
  )

  # estimate_type
  checkColumnContent(
    x = x, col = "estimate_type", content = estimateTypeChoices()
  )

  # settings
  sets <- settings(x)
  idSummary <- x |>
    dplyr::select("result_id") |>
    dplyr::distinct() |>
    dplyr::pull()
  idSettings <- sets |> dplyr::pull("result_id")
  if (length(idSettings) != length(unique(idSettings))) {
    cli::cli_abort("ids are not unique in settings")
  }
  notPresent <- idSummary[!idSummary %in% idSettings]
  if (length(notPresent) > 0) {
    cli::cli_abort("There are ids present in the summary that do not have settings: {paste0(notPresent, collapse = ', ')}")
  }

  # validate groupCount
  checkGroupCount(x)

  return(x)
}
