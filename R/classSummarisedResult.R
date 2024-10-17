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

#' 'summarised_results' object constructor
#'
#' @param x Table.
#' @param settings Settings for the summarised_result object.
#'
#' @return A `summarised_result` object
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(omopgenerics)
#'
#' x <- tibble(
#'  "result_id" = 1L,
#'  "cdm_name" = "cprd",
#'  "group_name" = "cohort_name",
#'  "group_level" = "acetaminophen",
#'  "strata_name" = "sex &&& age_group",
#'  "strata_level" = c("male &&& <40", "male &&& >=40"),
#'  "variable_name" = "number_subjects",
#'  "variable_level" = NA_character_,
#'  "estimate_name" = "count",
#'  "estimate_type" = "integer",
#'  "estimate_value" = c("5", "15"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult()
#'
#' x
#' settings(x)
#' summary(x)
#'
#' x <- tibble(
#'  "result_id" = 1L,
#'  "cdm_name" = "cprd",
#'  "group_name" = "cohort_name",
#'  "group_level" = "acetaminophen",
#'  "strata_name" = "sex &&& age_group",
#'  "strata_level" = c("male &&& <40", "male &&& >=40"),
#'  "variable_name" = "number_subjects",
#'  "variable_level" = NA_character_,
#'  "estimate_name" = "count",
#'  "estimate_type" = "integer",
#'  "estimate_value" = c("5", "15"),
#'   "additional_name" = "overall",
#'   "additional_level" = "overall"
#' ) |>
#'   newSummarisedResult(settings = tibble(
#'     result_id = 1L, result_type = "custom_summary", mock = TRUE, value = 5
#'   ))
#'
#' x
#' settings(x)
#' summary(x)
#'
newSummarisedResult <- function(x, settings = attr(x, "settings")) {

  # inital input check
  assertClass(x = x, class = "data.frame")
  assertClass(x = settings, class = "data.frame", null = TRUE)

  # constructor
  x <- constructSummarisedResult(x, settings)

  # validate
  x <- validateSummariseResult(x)

  return(x)
}

constructSummarisedResult <- function(x, set, call = parent.frame()) {
  x <- x |>
    dplyr::as_tibble()

  if (!is.null(set)) {
    set <- set |> dplyr::as_tibble()
    if (!"result_id" %in% colnames(set)) {
      cli::cli_abort("result_id must be a column of settings argument", call = call)
    }
  }

  settingsCols <- colnames(x)[
    !colnames(x) %in% resultColumns(table = "summarised_result")
  ]
  if (!"result_id" %in% colnames(x)) {
    x <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(settingsCols))) |>
      dplyr::mutate("result_id" = as.integer(dplyr::cur_group_id())) |>
      dplyr::ungroup()
    cli::cli_alert_warning("`result_id` column is missing, please add it as it is a compulsory column.")
  }
  x <- checkColumns(x = x, resultName = "summarised_result", call = call)
  if (length(settingsCols) > 0) {
    cli::cli_alert_warning("The following variables: {paste0(settingsCols, collapse = ', ')}; were added to `settings`")
    set <- set |>
      joinSet(
        x |>
          dplyr::select(dplyr::all_of(c("result_id", settingsCols))) |>
          dplyr::distinct()
      )
  }

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

  requiredSettingsColumns <- resultColumns(table = "settings")
  notPresent <- requiredSettingsColumns[
    !requiredSettingsColumns %in% colnames(set)]
  if (length(notPresent) > 0) {
    '{.var {notPresent}} {?is/are} not provided will be populated as "" in settings' |>
      cli::cli_warn()
    for (col in notPresent) {
      set <- set |>
        dplyr::mutate(!!col := "")
    }
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
joinSet <- function(set, addset) {
  if (is.null(set)) {
    set <- addset
  } else {
    set <- addset |> dplyr::full_join(set, by = "result_id")
  }
  return(set)
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

  # duplicated entries with same value
  nr <- nrow(x)
  x <- x |> dplyr::distinct()
  eliminated <- nr - nrow(x)
  if (eliminated > 0) {
    cli::cli_inform(c("!" = "{eliminated} duplicated row{?s} eliminated."))
  }

  # columPairs
  validateNameLevel(x = x, prefix = "group", validation = "warning")
  validateNameLevel(x = x, prefix = "strata", validation = "warning")
  validateNameLevel(x = x, prefix = "additional", validation = "warning")

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

  # validate duplicates
  checkDuplicated(x, validation = "error")

  # validate tidyNames
  validateTidyNames(x)

  return(x)
}
checkColumns <- function(x, resultName, call = parent.frame()) {
  cols <- resultColumns(table = resultName)
  notPresent <- cols[!cols %in% colnames(x)]
  if (length(notPresent) > 0) {
    cli::cli_abort(
      "{paste0(notPresent, collapse = ', ')} must be present in a {.cls {resultName}}
      object."
    )
  }
  x |> dplyr::relocate(dplyr::all_of(cols))
}
checkNA <- function(x, type, call = parent.frame()) {
  cols <- fieldsResults$result_field_name[
    fieldsResults$result == type & fieldsResults$na_allowed == FALSE
  ]
  for (col in cols) {
    if (any(is.na(unique(x[[col]])))) {
      cli::cli_abort("`{col}` must not contain NA.", call = call)
    }
  }
  invisible(NULL)
}
checkColumnsFormat <- function(x, resultName) {
  cols <- resultColumns(resultName)
  expectedFormat <- fieldsResults$datatype[fieldsResults$result == resultName]
  formats <- purrr::map_chr(x, typeof)
  id <- formats != expectedFormat
  cols <- cols[id]
  formats <- formats[id]
  expectedFormat <- expectedFormat[id]
  if (length(cols) > 0) {
    err <- character()
    for (k in seq_along(cols)) {
      res <- tryCatch(
        expr = {
          x <- x |>
            dplyr::mutate(!!cols[k] := giveType(.data[[cols[k]]], expectedFormat[k]))
          list(x = x, err = character())
        },
        error = function(e) {
          list(x = x, err = cols[k])
        }
      )
      x <- res$x
      err <- c(err, res$err)
    }
    if (length(err) > 0) {
      err <- paste0(err, ": format=", formats, " (expected=", expectedFormat, ")")
      names(err) <- rep("*", length(err))
      cli::cli_abort(c("The following colum does not have a correct format", err))
    } else {
      err <- paste0(cols, ": from ", formats, " to ", expectedFormat)
      names(err) <- rep("*", length(err))
      cli::cli_inform(c("!" = "The following column type were changed:", err))
    }
  }
  invisible(x)
}
checkGroupCount <- function(x, validation = "error", call = parent.frame()) {
  grouping <- c(
    "result_id", "cdm_name", "group_name", "group_level", "strata_name",
    "strata_level", "additional_name", "additional_level"
  )
  obsLabels <- x |>
    dplyr::pull("variable_name") |>
    unique()
  obsLabelsL <- obsLabels |>
    stringr::str_replace_all(pattern = "_", replacement = " ") |>
    tolower()
  res <- character()
  n <- 0
  for (gcount in groupCount) {
    if (n < 5) {
      ol <- obsLabels[obsLabelsL %in% gcount]
      xx <- x |>
        dplyr::filter(
          .data$variable_name %in% ol & grepl("count", .data$estimate_name)
        ) |>
        dplyr::select(dplyr::all_of(c(grouping, "variable_name"))) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::group_split() |>
        as.list()
      for (k in seq_along(xx)) {
        if (n < 5) {
          res <- c(res, "*" = glue::glue("{nrow(xx[[k]])} '{gcount}' in variable_name for: {getGrouping(xx[[k]])}."))
          n <- n + 1
        }
      }
    }
  }
  if (length(res) > 0) {
    res <- c(
      "Each grouping (unique combination of: {grouping}) can not contain repeated group identifiers ({groupCount}).",
      "First {n} combination{?s}:",
      res
    )
    cli::cli_abort(res)
  }
  return(invisible(NULL))
}
getGrouping <- function(x) {
  x <- x |>
    dplyr::select(-dplyr::any_of("variable_name")) |>
    dplyr::distinct() |>
    as.list()
  lapply(seq_along(x), function(kk) {
    paste0(names(x)[kk], ": ", x[[kk]])
  }) |>
    unlist() |>
    paste0(collapse = ", ")
}

#' Validate if two columns are valid Name-Level pair.
#'
#' @param x A tibble.
#' @param prefix Prefix for the name-level pair, e.g. 'strata' for
#' strata_name-strata_level pair.
#' @param sep Separation pattern.
#' @param validation Either 'error', 'warning' or 'message'.
#' @param call Will be used by cli to report errors.
#'
#' @export
#'
validateNameLevel <- function(x,
                              prefix,
                              sep = " &&& ",
                              validation = "error",
                              call = parent.frame()) {
  # inital checks
  assertCharacter(prefix, length = 1)
  nameColumn <- paste0(prefix, "_name")
  levelColumn <- paste0(prefix, "_level")
  assertTable(x, columns = c(nameColumn, levelColumn))
  assertCharacter(sep)
  assertValidation(validation)

  # distinct pairs
  distinctPairs <- x |>
    dplyr::select(
      "name" = dplyr::all_of(nameColumn), "level" = dplyr::all_of(levelColumn)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across(
      c("name", "level"),
      list(elements = ~ stringr::str_split(.x, pattern = sep))
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::ends_with("elements"),
      list(length = ~ lengths(.x))
    ))

  # pairs that dont match
  notMatch <- distinctPairs |>
    dplyr::filter(
      .data$name_elements_length != .data$level_elements_length
    )

  # error / warning
  if (nrow(notMatch) > 0) {
    unmatch <- notMatch |>
      dplyr::select("name", "level") |>
      dplyr::mutate("name_and_level" = paste0(
        .env$nameColumn, ": ", .data$name, "; ", .env$levelColumn, ": ",
        .data$level
      )) |>
      dplyr::pull("name_and_level")
    num <- length(unmatch)
    nun <- min(num, 5)
    unmatch <- unmatch[1:nun]
    names(unmatch) <- rep("*", nun)
    mes <- "name: `{nameColumn}` and level: `{levelColumn}` does not match in
    number of arguments ({num} unmatch), first {nun} unmatch:"

    # report
    report(c(mes, unmatch), validation = validation, call = call)
  }

  # check case
  nameCase <- distinctPairs[["name_elements"]] |> unlist() |> unique()
  notSnake <- nameCase[!isCase(nameCase, "snake")]
  if (length(notSnake) > 0) {
    "{length(notSnake)} element{?s} in {nameColumn} {?is/are} not snake_case." |>
      report(validation = validation, call = call)
  }

  return(invisible(x))
}
isCase <- function(x, case) {
  if (length(x) == 0) return(logical())
  flag <- switch(
    case,
    "snake" = isSnakeCase(x),
    "sentence" = isSentenceCase(x),
    "NA" = rep(TRUE, length(x)),
    rep(NA, length(x))
  )
  return(flag)
}
isSentenceCase <- function(x) {
  if (length(x) > 0) {
    x == snakecase::to_sentence_case(x)
  } else {
    x
  }
}
isSnakeCase <- function(x) {
  if (length(x) > 0) {
    x == toSnakeCase(x)
  } else {
    x
  }
}
checkColumnContent <- function(x, col, content) {
  if (!all(x[[col]] %in% content)) {
    notType <- x[[col]][!x[[col]] %in% content] |> unique()
    len <- length(notType)
    notType <- notType[1:min(5, len)]
    cli::cli_abort(c(
      "{col} contains incorrect values, possible values:
      {paste0(content, collapse = ', ')}. Observed values:
      {paste0(notType, collapse = ', ')}{ifelse(len>5, '...', '.')}"
    ))
  }
  return(invisible(TRUE))
}
checkDuplicated <- function(x, validation, call = parent.frame()) {
  nraw <- nrow(x)
  ndist <- x |>
    dplyr::select(!"estimate_value") |>
    dplyr::distinct() |>
    nrow()
  dup <- nraw - ndist
  if (dup > 0) {
    report(
      message = c(
        "{dup} duplicated results with different estimate values found.",
        "i" = "Run the following to see which are",
        "data |>",
        " " = "dplyr::group_by(dplyr::across(!'estimate_value')) |>",
        " " = "dplyr::tally() |>",
        " " = "dplyr::filter(n > 1)"
      ),
      validation = validation,
      call = call
    )
  }
  return(invisible(TRUE))
}
giveType <- function(x, type) {
  switch(
    type,
    "integer" = as.integer(x),
    "double" = as.double(x),
    "character" = as.character(x),
    "logical" = as.logical(x),
    x
  )
}
validateTidyNames <- function(result, call = parent.frame()) {
  # setting columns
  colsSettings <- colnames(settings(result))
  colsSettings <- colsSettings[colsSettings != "result_id"]

  # group columns
  colsGroup <- uniqueCols(result$group_name)

  # strata columns
  colsStrata <- uniqueCols(result$strata_name)

  # additional columns
  colsAdditional <- uniqueCols(result$additional_name)

  # default columns
  colsSummarisedResult <- resultColumns("summarised_result")

  cols <- list(
    settings = colsSettings,
    group = colsGroup,
    strata = colsStrata,
    additional = colsAdditional,
    summarised_result = colsSummarisedResult
  )

  # compare each pair
  len <- length(cols)
  nms <- names(cols)
  for (k in 1:(len -1)) {
    for (i in (k+1):len) {
      both <- intersect(cols[[k]], cols[[i]])
      if (length(both) > 0) {
        "{.var {both}} {?is/are} present in both '{nms[k]}' and '{nms[i]}'. This will be an error in the next release." |>
          cli::cli_warn() # Turn error
      }
    }
  }

  return(invisible(result))
}
uniqueCols <- function(x) {
  x <- x |>
    unique() |>
    stringr::str_split(" &&& ") |>
    unlist() |>
    unique()
  x[x != "overall"]
}

#' Required columns that the result tables must have.
#'
#' @param table Table to see required columns.
#'
#' @return Required columns
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' resultColumns()
#'
resultColumns <- function(table = "summarised_result") {
  assertChoice(table, unique(fieldsResults$result))
  x <- fieldsResults$result_field_name[fieldsResults$result == table]
  return(x)
}

#' Choices that can be present in `estimate_type` column.
#'
#' @return A character vector with the options that can be present in
#' `estimate_type` column in the summarised_result objects.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' estimateTypeChoices()
#'
estimateTypeChoices <- function() {
  c(
    "numeric", "integer", "date", "character", "proportion", "percentage",
    "logical"
  )
}

#' Empty `summarised_result` object.
#'
#' @param settings Tibble/data.frame with the settings of the empty
#' summarised_result. It has to contain at least `result_id` column.
#'
#' @return An empty `summarised_result` object.
#'
#' @export
#'
#' @examples
#' library(omopgenerics)
#'
#' emptySummarisedResult()
#'
emptySummarisedResult <- function(settings = NULL) {
  if (is.null(settings)) {
    settings <- dplyr::tibble(
      "result_id" = integer(),
      "result_type" = character(),
      "package_name" = character(),
      "package_version" = character()
    )
  }
  resultColumns("summarised_result") |>
    rlang::rep_named(list(character())) |>
    dplyr::as_tibble() |>
    dplyr::mutate("result_id" = as.integer()) |>
    newSummarisedResult(settings = settings)
}

.validateSummarisedResult <- function(result,
                                      checkNameLevelPairs,
                                      checkTidyColumns,
                                      checkDuplicated,
                                      call = parent.frame()) {
  # basic checks
  # class
  if (!inherits(result, "summarised_result")) {
    "result is not a {.cls summarised_result} object." |>
      report(validation = "error", call = call)
  }

  # compulsory columns
  result <- checkColumns(result, "summarised_result", call = call)

  # columns format
  result <- checkColumnsFormat(result, "summarised_result")

  # Cannot contain NA columns
  checkNA(result, "summarised_result")

  # duplicated entries with same value
  nr <- nrow(x)
  x <- x |> dplyr::distinct()
  eliminated <- nr - nrow(x)
  if (eliminated > 0) {
    "{eliminated} duplicated row{?s} eliminated." |>
      report(validation = "message", call = call)
  }

  # checkNameLevelPairs
  if (!is.null(checkNameLevelPairs)) {
    for (prefix in c("group", "strata", "additional")) {
      validateNameLevel(result, prefix = prefix, validation = checkNameLevelPairs)
    }
  }

  # checkTidyColumns

  # checkDuplicated


  return(result)
}
report <- function(message,
                   validation, # error/warning/inform
                   call = parent.frame(), # where error is reported
                   .envir = parent.frame()) { # where glue statements are evaluated
  if (validation == "error") {
    cli::cli_abort(addSignal(message, "x"), .envir = .envir, call = call)
  } else if (validation == "warning") {
    cli::cli_warn(addSignal(message, "!"), .envir = .envir)
  } else if (validation == "inform") {
    cli::cli_inform(addSignal(message, "!"), .envir = .envir)
  }
  return(invisible(TRUE))
}
addSignal <- function(x, nm) {
  if (length(x) > 0) names(x)[1] <- nm
  return(x)
}
