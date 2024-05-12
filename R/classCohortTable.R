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

#' `cohort_table` objects constructor.
#'
#' @param table cdm_table object with at least: cohort_definition_id,
#' subject_id, cohort_start_date, cohort_end_date.
#' @param cohortSetRef Table with at least: cohort_definition_id, cohort_name
#' @param cohortAttritionRef Table with at least: cohort_definition_id,
#' number_subjects, number_records, reason_id, reason, excluded_subjects,
#' excluded_records.
#' @param cohortCodelistRef Table with at least: cohort_definition_id, codelist_name,
#' and concept_id.
#' @param .softValidation Whether to perform a soft validation of consistency.
#' If set to FALSE four additional checks will be performed: 1) a check that
#' cohort end date is not before cohort start date,  2) a check that there
#' are no missing values in required columns, 3) a check that cohort duration is
#' all within observation period, and 4) that there are no overlapping
#' cohort entries
#'
#' @return A cohort_table object
#'
#' @export
#'
#' @examples
#' person <- dplyr::tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- dplyr::tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2025-12-31"),
#'   period_type_concept_id = 0
#' )
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = 1, subject_id = 1,
#'   cohort_start_date = as.Date("2020-01-01"),
#'   cohort_end_date = as.Date("2020-01-10")
#' )
#' cdm <- cdmFromTables(
#'   tables = list(
#'     "person" = person,
#'     "observation_period" = observation_period,
#'     "cohort1" = cohort1
#'   ),
#'   cdmName = "test"
#' )
#' cdm
#' cdm$cohort1 <- newCohortTable(table = cdm$cohort1)
#' cdm
#' settings(cdm$cohort1)
#' attrition(cdm$cohort1)
#' cohortCount(cdm$cohort1)
#'
newCohortTable <- function(table,
                           cohortSetRef = attr(table, "cohort_set"),
                           cohortAttritionRef = attr(table, "cohort_attrition"),
                           cohortCodelistRef = attr(table, "cohort_codelist"),
                           .softValidation = FALSE) {
  # initial checks
  assertClass(table, "cdm_table")
  assertChoice(.softValidation, choices = c(TRUE, FALSE), length = 1)

  # populate
  cohortSetRef <- populateCohortSet(table, cohortSetRef)
  cohortAttritionRef <- populateCohortAttrition(
    table, cohortSetRef, cohortAttritionRef
  )
  cohortCodelistRef <- populateCohortCodelist(table, cohortCodelistRef)

  # constructor
  cohort <- constructGeneratedCohortSet(
    table = table,
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef,
    cohortCodelistRef = cohortCodelistRef
  )

  # validate
  cohort <- validateGeneratedCohortSet(cohort, soft = .softValidation)

  # return
  return(cohort)
}

#' To collect a `cohort_table` object.
#'
#' @param x `cohort_table` object.
#' @param ... Not used (for compatibility).
#'
#' @return A data frame with the `cohort_table`
#'
#' @export
#'
#' @importFrom dplyr collect
#'
collect.cohort_table <- function(x, ...) {
  x <- removeClass(x, "cohort_table")
  y <- x |> dplyr::collect()
  attr(y, "cohort_set") <- attr(x, "cohort_set") |> dplyr::collect()
  attr(y, "cohort_attrition") <- attr(x, "cohort_attrition") |> dplyr::collect()
  return(y)
}

constructGeneratedCohortSet <- function(table,
                                        cohortSetRef,
                                        cohortAttritionRef,
                                        cohortCodelistRef) {
  table <- structure(
    .Data = table,
    "cohort_set" = noReference(cohortSetRef),
    "cohort_attrition" = noReference(cohortAttritionRef),
    "cohort_codelist" = noReference(cohortCodelistRef)
  ) |>
    addClass(c("cohort_table", "GeneratedCohortSet"))
  return(table)
}
validateGeneratedCohortSet <- function(cohort, soft = FALSE) {
  # get attributes
  cohort_set <- attr(cohort, "cohort_set")
  cohort_attrition <- attr(cohort, "cohort_attrition")
  cohort_codelist <- attr(cohort, "cohort_codelist")

  # assertClass
  assertClass(cohort, "cdm_table")
  assertClass(cohort_set, "cdm_table")
  assertClass(cohort_attrition, "cdm_table")
  assertClass(cohort_codelist, "cdm_table")

  # check cdm reference
  if (!"cdm_reference" %in% names(attributes(cohort))) {
    cli::cli_abort("cohort must be part of a cdm_reference")
  }

  # check name
  assertCharacter(tableName(cohort), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_set), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_attrition), length = 1, na = TRUE)
  assertCharacter(tableName(cohort_codelist), length = 1, na = TRUE)
  consistentNaming(
    cohortName = tableName(cohort),
    cohortSetName = tableName(cohort_set),
    cohortAttritionName = tableName(cohort_attrition),
    cohortCodelistName = tableName(cohort_codelist)
  )

  # check source
  srcCohort <- tableSource(cohort)
  srcCohortSet <- tableSource(cohort_set)
  srcCohortAttrition <- tableSource(cohort_attrition)
  srcCohort_codelist <- tableSource(cohort_codelist)
  if (!equal(srcCohort, srcCohortSet, srcCohortAttrition, srcCohort_codelist)) {
    cli::cli_abort(
      "The source must be the same for cohort, cohort_set, and cohort_attrition."
    )
  }

  # assert columns
  checkColumnsCohort <- function(x, nam) {
    cols <- cohortColumns(nam)
    if (!all(cols %in% colnames(x))) {
      cli::cli_abort(paste0(
        "`", paste0(cols, collapse = "`, `"), "` must be column names of the ",
        nam, " of a cohort_table object."
      ))
    }
    invisible(NULL)
  }
  checkColumnsCohort(cohort, "cohort")
  checkColumnsCohort(cohort_set, "cohort_set")
  checkColumnsCohort(cohort_attrition, "cohort_attrition")
  checkColumnsCohort(cohort_codelist, "cohort_codelist")

  # check cohort_codelist type colum
  if(cohort_codelist |>
     utils::head(10) |>
     dplyr::tally() |>
     dplyr::pull("n") > 0){
  checkCodelistType(cohort_codelist)
  }

  # cohort_definition_id is coherent
  cdiCohort <- cdi(cohort)
  cdiCohortSet <- cdi(cohort_set)
  cdiCohortAttrition <- cdi(cohort_attrition)
  cdiCohortCodelist <- cdi(cohort_codelist)
  if (!all(cdiCohortSet == cdiCohortAttrition)) {
    cli::cli_abort(c(
      "Present cohort_definition_id must be the same:",
      "*" = "cohort_set: {cdiCohortSet}",
      "*" = "cohort_attrition: {cdiCohortAttrition}"
    ))
  }
  if (!all(cdiCohort %in% cdiCohortSet)) {
    cli::cli_abort(c(
      "There are cohort_definition_id that appear in cohort and not in cohort_set:",
      "*" = "cohort: {paste0(cdiCohort, collapse = ', ')}",
      "*" = "cohort_set: {paste0(cdiCohortSet, collapse = ', ')}"
    ))
  }
  if (!all(cdiCohortCodelist %in% cdiCohortSet)) {
    cli::cli_abort(c(
      "There are cohort_definition_id that appear in cohort and not in cohort_codelist:",
      "*" = "cohort_codelist: {paste0(cdiCohortCodelist, collapse = ', ')}",
      "*" = "cohort_set: {paste0(cdiCohortSet, collapse = ', ')}"
    ))
  }

  # cohort_name column
  cohortNames <- cohort_set |> dplyr::pull("cohort_name")
  if (length(cohortNames) != length(unique(cohortNames))) {
    cli::cli_abort("cohort_name in the cohort_set must be unique")
  }
  notSnake <- cohortNames[!isSnakeCase(cohortNames)]
  if (length(notSnake)) {
    oldName <- notSnake
    newName <- toSnakeCase(notSnake)
    x <- paste0(oldName, " -> ", newName)
    names(x) <- rep("*", length(x))
    cli::cli_warn(c(
      "cohort_name must be snake case, the following cohorts will be renamed:",
      x
    ))
    nameChange <- dplyr::tibble(
      "xyz_cohort_name" = newName, "cohort_name" = oldName
    )
    attr(cohort, "cohort_set") <- attr(cohort, "cohort_set") |>
      dplyr::left_join(nameChange, by = "cohort_name", copy = TRUE) |>
      dplyr::mutate("cohort_name" = dplyr::if_else(
        is.na(.data$xyz_cohort_name), .data$cohort_name, .data$xyz_cohort_name
      )) |>
      dplyr::select(-"xyz_cohort_name") |>
      dplyr::compute(
        name = paste0(tableName(cohort), "_set"), temporary = FALSE,
        overwrite = TRUE
      )
  }

  # make correct order
  cohort <- cohort |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort")))
  attr(cohort, "cohort_set") <- attr(cohort, "cohort_set") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_set")))
  attr(cohort, "cohort_attrition") <- attr(cohort, "cohort_attrition") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_attrition")))
  attr(cohort, "cohort_codelist") <- attr(cohort, "cohort_codelist") |>
    dplyr::relocate(dplyr::all_of(cohortColumns("cohort_codelist")))

  if (!soft) {
    checkCohortRequirements(cohort,
                            checkEndAfterStart = TRUE, # check start before end
                            checkMissingValues = TRUE, # check NA
                            checkOverlappingEntries = TRUE, # check overlap
                            checkInObservation = TRUE) # check within observation period
  }

  return(cohort)
}
equal <- function(...) {
  x <- list(...)
  flag <- TRUE
  for (k in 2:length(x)) {
    flag <- flag & all(x[[1]]==x[[k]])
  }
  return(flag)
}
cl <- function(x) {
  x <- class(x)
  x <- x[!grepl(pattern = "cohort", x = x, ignore.case = TRUE)]
  paste0(x, collapse = ", ")
}
cdi <- function(x) {
  x |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::pull() |>
    sort()
}
defaultCohortSet <- function(cohort) {
  cohortName <- tableName(cohort)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_set"))
  cohort |>
    dplyr::select("cohort_definition_id") |>
    dplyr::distinct() |>
    dplyr::mutate(
      "cohort_definition_id" = as.integer(.data$cohort_definition_id),
      "cohort_name" = paste0("cohort_", as.character(.data$cohort_definition_id))
    ) |>
    collect()
}
defaultCohortAttrition <- function(cohort, set) {
  cohortName <- tableName(cohort)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_attrition"))
  x <- cohort |>
    group_by(.data$cohort_definition_id) |>
    summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    dplyr::right_join(
      set |> dplyr::select("cohort_definition_id"),
      by = "cohort_definition_id",
      copy = TRUE
    ) |>
    dplyr::mutate(
      "number_records" = dplyr::if_else(
        is.na(.data$number_records), 0, .data$number_records
      ),
      "number_subjects" = dplyr::if_else(
        is.na(.data$number_subjects), 0, .data$number_subjects
      ),
      "reason_id" = 1,
      "reason" = "Initial qualifying events",
      "excluded_records" = 0,
      "excluded_subjects" = 0
    ) |>
    collect()
  return(x)
}
defaultCohortCodelist <- function(cohort) {
  dplyr::tibble(
    cohort_definition_id = as.integer(),
    codelist_name = as.character(),
    concept_id = as.integer(),
    type = as.character()
  )
}


#' Check whether a cohort table satisfies requirements
#'
#' @param cohort `cohort_table` object.
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on or
#' after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have overlapping
#' cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#' @param type Can be either "error" or "warning". If "error" any check
#' failure will result in an error, whereas if "warning" any check failure
#' will result in a warning.
#' @param call The call for which to return the error message.
#'
#' @return An error will be returned if any of the selected checks fail.
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' person <- dplyr::tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- dplyr::tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2025-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#' cdm <- insertTable(cdm, name = "cohort1", table = dplyr::tibble(
#'   cohort_definition_id = 1, subject_id = 1,
#'   cohort_start_date = as.Date(c("2020-01-01", "2020-01-10")),
#'   cohort_end_date = as.Date(c("2020-01-10", "2020-01-25"))
#' ))
#' cdm$cohort1 <- newCohortTable(cdm$cohort1, .softValidation = TRUE)
#' # checkCohortRequirements(cdm$cohort1)
#' }
checkCohortRequirements <- function(cohort,
                                    checkEndAfterStart = TRUE,
                                    checkOverlappingEntries = TRUE,
                                    checkMissingValues = TRUE,
                                    checkInObservation = TRUE,
                                    type = "error",
                                    call = parent.frame()){

  assertClass(cohort, "cdm_table")
  assertClass(cohort, "cohort_table")

  if(!is.logical(checkEndAfterStart)){
    cli::cli_abort("checkEndAfterStart must be TRUE or FALSE")
  }
  if(!is.logical(checkOverlappingEntries)){
    cli::cli_abort("checkOverlappingEntries must be TRUE or FALSE")
  }
  if(!is.logical(checkMissingValues)){
    cli::cli_abort("checkMissingValues must be TRUE or FALSE")
  }
  if(!is.logical(checkInObservation)){
    cli::cli_abort("checkInObservation must be TRUE or FALSE")
  }

  if(isTRUE(checkEndAfterStart)){
    checkStartEnd(cohort = cohort, type = type, call = call)
  }
  if(isTRUE(checkOverlappingEntries)){
    checkOverlap(cohort = cohort, type = type, call = call)
  }
  if(isTRUE(checkMissingValues)){
    checkNaCohort(cohort = cohort, type = type, call = call)
  }
  if(isTRUE(checkInObservation)){
    checkObservationPeriod(cohort = cohort, type = type, call = call)
  }

  return(invisible(TRUE))

}

checkStartEnd <- function(cohort, type = "error", call = parent.frame()) {
  x <- cohort |>
    dplyr::filter(.data$cohort_end_date < .data$cohort_start_date) |>
    dplyr::collect()
  if (nrow(x) > 0){
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(cohortColumns("cohort"))) |>
      utils::head(5) |>
      dplyr::glimpse() |>
      print(width = Inf) |>
      utils::capture.output()
    if(type == "error"){
      cli::cli_abort(
        message = c(
          "cohort_start_date must be <= tham cohort_end_date. There are {nrow(x)}
        entries where cohort_end_date < cohort_start_date
        {ifelse(nrow(x)<=5, ':', ' first 5:')}",
          x5[3:7]
        ),
        call = call
      )
    } else {
      cli::cli_warn(
        message = c(
          "cohort_start_date must be <= tham cohort_end_date. There are {nrow(x)}
        entries where cohort_end_date < cohort_start_date
        {ifelse(nrow(x)<=5, ':', ' first 5:')}",
          x5[3:7]
        )
      )
    }

  }
  return(invisible(TRUE))
}
checkOverlap <- function(cohort, type = "error", call = parent.frame()) {
  x <- cohort |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(
      "next_cohort_start_date" = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::filter(.data$cohort_end_date >= .data$next_cohort_start_date) |>
    dplyr::collect()
  if (nrow(x) > 0){
    x5 <- x |>
      dplyr::ungroup() |>
      dplyr::select(
        "cohort_definition_id", "subject_id", "cohort_start_date",
        "cohort_end_date", "next_cohort_start_date"
      ) |>
      utils::head(5) |>
      dplyr::glimpse() |>
      print(width = Inf) |>
      utils::capture.output()


    if(type == "error"){
    cli::cli_abort(
      message = c(
        "There is overlap between entries in the cohort, {nrow(x)} overlap{?s}
        detected{ifelse(nrow(x)<=5, ':', ' first 5:')}",
        x5[3:7]
      ),
      call = call
    )
      } else {
      cli::cli_warn(
        message = c(
          "There is overlap between entries in the cohort, {nrow(x)} overlap{?s}
        detected{ifelse(nrow(x)<=5, ':', ' first 5:')}",
          x5[3:7]
        )
      )
    }


  }
  return(invisible(TRUE))
}
checkNaCohort <- function(cohort, type = "error", call = parent.frame()) {
  x <- cohort |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ is.na(.x))) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    x <- x |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(is.na(.data$value)) |>
      dplyr::pull("name") |>
      unique() |>
      paste0(collapse = ", ")

    if(type == "error"){
    cli::cli_abort(
      "Cohort can't have NA values, there are NA values in the following
      columns: {x}",
      call = call
    )} else {
      cli::cli_warn(
        "Cohort can't have NA values, there are NA values in the following
      columns: {x}"
      )
      }
  }
  return(invisible(TRUE))
}
checkObservationPeriod <- function(cohort, type = "error", call = parent.frame()) {
  cdm <- attr(cohort, "cdm_reference")
  x <- cohort |>
    dplyr::anti_join(
      cohort |>
        dplyr::select(dplyr::all_of(cohortColumns("cohort"))) |>
        dplyr::left_join(
          cdm[["observation_period"]] |>
            dplyr::select(
              "subject_id" = "person_id", "observation_period_start_date",
              "observation_period_end_date"
            ),
          by = "subject_id"
        ) |>
        dplyr::filter(
          .data$cohort_start_date >= .data$observation_period_start_date &
            .data$cohort_start_date <= .data$observation_period_end_date &
            .data$cohort_end_date >= .data$observation_period_start_date &
            .data$cohort_end_date <= .data$observation_period_end_date
        ),
      by = cohortColumns("cohort")
    ) |>
    dplyr::collect()

  if (nrow(x) > 0) {
    if(type == "error"){
    cli::cli_abort(
      message = "{nrow(x)} observation{?s} outside observation period.",
      call = call
    )
  } else {
    cli::cli_warn(
      message = "{nrow(x)} observation{?s} outside observation period."
    )
  }


  }
  return(invisible(TRUE))
}
checkCodelistType <- function(cohort_codelist){
codelist_types <- cohort_codelist |>
    dplyr::pull("type")
assertChoice(codelist_types,
               c("index event",
                       "inclusion criteria",
                       "exit criteria"))
}
consistentNaming <- function(cohortName,
                             cohortSetName,
                             cohortAttritionName,
                             cohortCodelistName) {
  if (is.na(cohortName)) {
    if (!is.na(cohortSetName) | !is.na(cohortAttritionName)) {
      cli::cli_abort("cohort is a temp table, cohort_set and cohort_attrition should be a temp table too")
    }
  } else {
    errorMessage <- character()
    if (cohortSetName != paste0(cohortName, "_set")) {
      errorMessage <- c(errorMessage, "cohort_set name must be {paste0(cohortName, '_set')} but is {cohortSetName}")
    }
    if (cohortAttritionName != paste0(cohortName, "_attrition")) {
      errorMessage <- c(errorMessage, "cohort_attrition name must be {paste0(cohortName, '_attrition')} but is {cohortAttritionName}")
    }
    if (cohortCodelistName != paste0(cohortName, "_codelist")) {
      errorMessage <- c(errorMessage, "cohort_codelist name must be {paste0(cohortName, '_codelist')} but is {cohortCodelistName}")
    }
    if (length(errorMessage) > 0) {
      cli::cli_abort(errorMessage)
    }
  }
  return(invisible(TRUE))
}
populateCohortSet <- function(table, cohortSetRef) {
  if (is.null(cohortSetRef)) {
    cohortSetRef <- defaultCohortSet(table)
  } else {
    cohortSetRef <- cohortSetRef |> dplyr::collect()
  }
  cohortName <- tableName(table)
  assertClass(cohortSetRef, "data.frame", null = TRUE)
  cohortSetRef <- dplyr::as_tibble(cohortSetRef)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_set"))
  cohortSetRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortSetRef,
    overwrite = TRUE
  )
  return(cohortSetRef)
}
populateCohortAttrition <- function(table, cohortSetRef, cohortAttritionRef) {
  if (is.null(cohortAttritionRef)) {
    cohortAttritionRef <- defaultCohortAttrition(table, cohortSetRef)
  } else {
    cohortAttritionRef <- cohortAttritionRef |> dplyr::collect()
  }
  cohortName <- tableName(table)
  assertClass(cohortAttritionRef, "data.frame", null = TRUE)
  cohortAttritionRef <- dplyr::as_tibble(cohortAttritionRef)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_attrition"))
  cohortAttritionRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortAttritionRef,
    overwrite = TRUE
  )
  return(cohortAttritionRef)
}
populateCohortCodelist <- function(table, cohortCodelistRef) {
  if (is.null(cohortCodelistRef)) {
    cohortCodelistRef <- defaultCohortCodelist(table)
  } else {
    cohortCodelistRef <- cohortCodelistRef |> dplyr::collect()
  }
  cohortName <- tableName(table)
  assertClass(cohortCodelistRef, "data.frame", null = TRUE)
  cohortCodelistRef <- dplyr::as_tibble(cohortCodelistRef)
  name <- ifelse(is.na(cohortName), cohortName, paste0(cohortName, "_codelist"))
  cohortCodelistRef <- insertTable(
    cdm = tableSource(table), name = name, table = cohortCodelistRef,
    overwrite = TRUE
  )
  return(cohortCodelistRef)
}

#' Create an empty cohort_table object
#'
#' @param cdm A cdm_reference to create the table.
#' @param name Name of the table to create.
#' @param overwrite Whether to overwrite an existent table.
#'
#' @export
#'
#' @return The cdm_reference with an empty cohort table
#'
#' @examples
#' library(omopgenerics)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' person <- tibble(
#'   person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
#'   race_concept_id = 0, ethnicity_concept_id = 0
#' )
#' observation_period <- tibble(
#'   observation_period_id = 1, person_id = 1,
#'   observation_period_start_date = as.Date("2000-01-01"),
#'   observation_period_end_date = as.Date("2025-12-31"),
#'   period_type_concept_id = 0
#' )
#' cdm <- cdmFromTables(
#'   tables = list("person" = person, "observation_period" = observation_period),
#'   cdmName = "test"
#' )
#'
#' cdm <- emptyCohortTable(cdm, "my_empty_cohort")
#'
#' cdm
#' cdm$my_empty_cohort
#' settings(cdm$my_empty_cohort)
#' attrition(cdm$my_empty_cohort)
#' cohortCount(cdm$my_empty_cohort)
#'
emptyCohortTable <- function(cdm, name, overwrite = FALSE) {
  assertCharacter(name, length = 1)
  assertClass(cdm, "cdm_reference")
  table <- fieldsTables |>
    dplyr::filter(
      .data$cdm_table_name == "cohort" &
        .data$type == "cohort" &
        grepl(cdmVersion(cdm), .data$cdm_version)
    ) |>
    emptyTable()
  cdm <- insertTable(cdm = cdm, name = name, table = table, overwrite = overwrite)
  cdm[[name]] <- newCohortTable(cdm[[name]], .softValidation = TRUE)
  return(cdm)
}

emptyTable <- function(fields) {
  lapply(fields$cdm_datatype, getEmptyField) |>
    rlang::set_names(fields$cdm_field_name) |>
    dplyr::as_tibble()
}
getEmptyField <- function(datatype) {
  datatype[grepl("varchar", datatype)] <- "varchar"
  empty <- switch(
    datatype,
    "integer" = integer(),
    "datetime" = as.Date(integer()),
    "date" = as.Date(integer()),
    "float" = numeric(),
    "varchar" = character(),
    "logical" = logical()
  )
  return(empty)
}
