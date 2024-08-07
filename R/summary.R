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

#' Summary a cdm reference
#'
#' @param object A cdm reference object.
#' @param ... For compatibility (not used).
#'
#' @return A summarised_result object with a summary of the data contained in
#' the cdm.
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2023-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#'
#' summary(cdm)
#'
summary.cdm_reference <- function(object, ...) {
  # snapshot date
  snapshotDate <- dplyr::tibble(
    "variable_name" = "snapshot_date",
    "estimate_name" = "value",
    "estimate_type" = "date",
    "estimate_value" = as.character(format(Sys.Date(), "%Y-%m-%d"))
  )

  # person count
  personCount <- dplyr::tibble(
    "variable_name" = "person_count",
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = object[["person"]] |>
      dplyr::tally() |>
      dplyr::pull("n") |>
      as.character()
  )

  # observation period info
  if (object[["observation_period"]] |> dplyr::tally() |> dplyr::pull() > 0) {
    observation_period_info <- object[["observation_period"]] |>
      dplyr::summarise(
        "count" = dplyr::n(),
        "max" = max(.data$observation_period_end_date, na.rm = TRUE),
        "min" = min(.data$observation_period_start_date, na.rm = TRUE)
      ) |>
      dplyr::collect() |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  } else {
    observation_period_info <- dplyr::tibble(
      "count" = "0", "max" = NA_character_, "min" = NA_character_
    )
  }

  # observation period count
  observationPeriodCount <- dplyr::tibble(
    "variable_name" = "observation_period_count",
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = observation_period_info$count
  )

  # cdm source data
  vocab_version <- getVocabularyVersion(object)
  if ("cdm_source" %in% names(object) &&
      object[["cdm_source"]] |> dplyr::tally() |> dplyr::pull("n") == 1) {
    cdmSourceSummary <- object[["cdm_source"]] |>
      dplyr::collect() |>
      dplyr::select(dplyr::any_of(c(
        "vocabulary_version", "cdm_source_name",
        "cdm_holder_name" = "cdm_holder", "cdm_release_date", "cdm_version",
        "cdm_description" = "source_description",
        "cdm_documentation_reference" = "source_documentation_reference"
      )))
    if (!"vocabulary_version" %in% colnames(cdmSourceSummary)) {
      cdmSourceSummary <- cdmSourceSummary |>
        dplyr::mutate("vocabulary_version" = vocab_version)
    }
    if (!"cdm_version" %in% colnames(cdmSourceSummary)) {
      cdmSourceSummary <- cdmSourceSummary |>
        dplyr::mutate(
          "cdm_version" = dplyr::coalesce(cdmVersion(object), "")
        )
    }
    cols <- c(
      "cdm_source_name", "cdm_holder_name", "cdm_release_date",
      "cdm_description", "cdm_documentation_reference"
    )
    for (col in cols) {
      if (!col %in% colnames(cdmSourceSummary)) {
        cdmSourceSummary <- cdmSourceSummary |> dplyr::mutate(!!col := "")
      }
    }
  } else {
    cdmSourceSummary <- dplyr::tibble(
      vocabulary_version = vocab_version,
      cdm_source_name = "",
      cdm_holder_name = "",
      cdm_release_date = "",
      cdm_version = dplyr::coalesce(cdmVersion(object), ""),
      cdm_description = "",
      cdm_documentation_reference = ""
    )
  }
  cdmSourceSummary <- cdmSourceSummary |>
    dplyr::select(dplyr::all_of(c(
      "cdm_source_name", "vocabulary_version", "cdm_version", "cdm_holder_name",
      "cdm_release_date", "cdm_description", "cdm_documentation_reference"
    ))) |>
    dplyr::mutate("cdm_source_type" = sourceType(object)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), names_to = "variable",
      values_to = "estimate_value"
    ) |>
    tidyr::separate_wider_delim(
      cols = c("variable"),
      delim = "_",
      names = c("variable_name", "estimate_name"),
      too_many = "merge",
      cols_remove = TRUE
    ) |>
    dplyr::mutate(estimate_type = "character")

  # observation periods values
  observationPeriodValues <- dplyr::tibble(
    "variable_name" = c(
      "observation_period_start_date", "observation_period_end_date"
    ),
    "estimate_name" = c("min", "max"),
    "estimate_type" = "date",
    "estimate_value" = c(
      observation_period_info$min, observation_period_info$max
    )
  )

  # merge snapshot
  x <- snapshotDate |>
    dplyr::union_all(personCount) |>
    dplyr::union_all(observationPeriodCount) |>
    dplyr::union_all(cdmSourceSummary) |>
    dplyr::union_all(observationPeriodValues) |>
    dplyr::mutate(
      "result_id" = 1L,
      "cdm_name" = cdmName(object),
      "variable_level" = NA_character_,
      "group_name" = "overall",
      "group_level" = "overall",
      "strata_name" = "overall",
      "strata_level" = "overall",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    dplyr::select(dplyr::all_of(resultColumns("summarised_result"))) |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 1L,
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics")),
      "result_type" = "cdm_snapshot"
    ))

  return(x)
}

#' Summary a generated cohort set
#'
#' @param object A generated cohort set object.
#' @param ... For compatibility (not used).
#'
#' @return A summarised_result object with a summary of a cohort_table.
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2023-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test",
#'   cohortTables = list("cohort1" = tibble(
#'     cohort_definition_id = 1,
#'     subject_id = 1,
#'     cohort_start_date = as.Date("2010-01-01"),
#'     cohort_end_date = as.Date("2010-01-05")
#'   ))
#' )
#'
#' summary(cdm$cohort1)
#'
summary.cohort_table <- function(object, ...) {
  if (is.null(cdmReference(object))) {
    cli::cli_abort(
      "Can't find the cdm that this cohort comes from (cdmReference(object) is
      NULL)."
    )
  }
  if (is.null(tableName(object))) {
    cli::cli_abort(
      "Can't find the table name of this cohort (tableName(object) is NULL)."
    )
  }

  # settings part
  settingsSummary <- settings(object) |>
    dplyr::mutate(
      "table_name" = tableName(object),
      "result_id" = as.integer(.data$cohort_definition_id)
    )

  # counts summary
  countsSummary <- cohortCount(object) |>
    dplyr::inner_join(
      settingsSummary |>
        dplyr::select(
          "group_level" = "cohort_name", "cohort_definition_id", "result_id"
        ),
      by = "cohort_definition_id"
    ) |>
    dplyr::select(-"cohort_definition_id") |>
    dplyr::mutate(dplyr::across(!"result_id", as.character)) |>
    tidyr::pivot_longer(
      cols = !c("group_level", "result_id"),
      names_to = "variable_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "variable_level" = NA_character_,
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "group_name" = "cohort_name",
      "strata_name" = "overall",
      "strata_level" = "overall",
      "additional_name" = "overall",
      "additional_level" = "overall",
      "cdm_name" = object |> cdmReference() |> cdmName()
    ) |>
    newSummarisedResult(
      settings = settingsSummary |>
        dplyr::mutate("result_type" = "cohort_count") |>
        addPkgDetails()
    )

  # attrition summary
  attritionSummary <- attrition(object) |>
    dplyr::inner_join(
      settingsSummary |>
        dplyr::select(
          "group_level" = "cohort_name", "cohort_definition_id", "result_id"
        ),
      by = "cohort_definition_id"
    ) |>
    dplyr::select(-"cohort_definition_id") |>
    dplyr::mutate(dplyr::across(!"result_id", as.character)) |>
    tidyr::pivot_longer(
      cols = c(
        "number_records", "number_subjects", "excluded_records",
        "excluded_subjects"
      ),
      names_to = "variable_name", values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "group_name" = "cohort_name",
      "estimate_name" = "count",
      "variable_level" = NA_character_,
      "estimate_type" = "integer",
      "strata_name" = "reason",
      "strata_level" = .data$reason,
      "additional_name" = "reason_id",
      "additional_level" = as.character(.data$reason_id),
      "cdm_name" = object |> cdmReference() |> cdmName()
    ) |>
    dplyr::select(-c("reason_id", "reason")) |>
    newSummarisedResult(
      settings = settingsSummary |>
        dplyr::mutate("result_type" = "cohort_attrition") |>
        addPkgDetails()
    )

  x <- bind(countsSummary, attritionSummary)

  return(x)
}

addPkgDetails <- function(res) {
  res |>
    dplyr::mutate(
      "package_name" = "omopgenerics",
      "package_version" = as.character(utils::packageVersion("omopgenerics"))
    )
}

#' Summary a summarised_result
#'
#' @param object A summarised_result object.
#' @param ... For compatibility (not used).
#'
#' @return A summary of the result_types contained in a summarised_result
#' object.
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#'  )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2023-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#'
#' result <- summary(cdm)
#'
#' summary(result)
#'
summary.summarised_result <- function(object, ...) {
  cdms <- object$cdm_name |> unique()
  cdms <- cdms[!is.na(cdms)]
  ids <- object$result_id |> unique()
  set <- object |> settings() |> colnames()
  set <- set[set != "result_id"]
  cli::cli_inform(
    "A summarised_result object with {nrow(object)} rows, {length(ids)}
    different result_id, {lengths(cdms)} different cdm names, and {length(set)}
    settings."
  )
  if (length(cdms) > 0) {
    cli::cli_inform("CDM names: {cdms}.")
  }
  if (length(set) > 0) {
    cli::cli_inform("Settings: {set}.")
  }
}

getTypes <- function(x) {
  for (col in colnames(x)) {
    x <- x |> dplyr::mutate(!!col := getType(x[[col]]))
  }
  x |>
    utils::head(1) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), names_to = "estimate_name",
      values_to = "estimate_type"
    )
}
getType <- function(x) {
  if (is.numeric(x)) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return("numeric")
    } else if (all(round(x) == floor(x))) {
      return("integer")
    } else {
      return("numeric")
    }
  } else if (is.logical(x)) {
    return("logical")
  } else if (methods::is(x, "Date")) {
    return("date")
  } else if (is.character(x)) {
    return("character")
  } else {
    cli::cli_abort(
      "Can't assign the type of {x}, please report it if you think it is
      mistake. Supported types are integer, numeric, logical, date and
      character."
    )
  }
}
