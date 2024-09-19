# Copyright 2024 DARWIN EU (C)
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

#' Validate name argument.
#'
#' @param name Name of a new table to be added to a cdm object.
#' @param cdm A cdm_reference object. It will check if a table named name
#' already exists in the cdm.
#' @param null If TRUE, name can be NULL
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateNameArgument <- function(name,
                                 cdm = NULL,
                                 validation = "error",
                                 null = FALSE,
                                 call = parent.frame()) {
  assertValidation(validation)
  if(null == TRUE && is.null(name)){
    return(NULL)
  }

  nm <- substitute(name) |> utils::capture.output()
  assertCharacter(name, length = 1, call = call)
  newName <- toSnakeCase(name)
  if (newName != name) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "`{nm}` is not snake_case it should be modified to: {newName}"))
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "`{nm}` was modified: {name} -> {newName}"))
    }
  }
  if (!is.null(cdm)) {
    if (newName %in% names(cdm)) {
      if (validation == "error") {
        cli::cli_abort(c("!" = "There already exist a table named {.var {newName}}."))
      } else if (validation == "warning") {
        cli::cli_warn(c("!" = "There already exist a table named {.var {newName}}. It will be overwritten."))
      }
    }
  }
  return(newName)
}

#' Validate a cohort table input.
#'
#' @param cohort Object to be validated as a valid cohort input.
#' @param checkEndAfterStart If TRUE a check that all cohort end dates come on
#' or after cohort start date will be performed.
#' @param checkOverlappingEntries If TRUE a check that no individuals have
#' overlapping cohort entries will be performed.
#' @param checkMissingValues If TRUE a check that there are no missing values in
#' required fields will be performed.
#' @param checkInObservation If TRUE a check that cohort entries are within
#' the individuals observation periods will be performed.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateCohortArgument <- function(cohort,
                                   checkEndAfterStart = FALSE,
                                   checkOverlappingEntries = FALSE,
                                   checkMissingValues = FALSE,
                                   checkInObservation = FALSE,
                                   validation = "error",
                                   call = parent.frame()) {
  assertValidation(validation)
  assertLogical(checkEndAfterStart, length = 1)
  assertLogical(checkOverlappingEntries, length = 1)
  assertLogical(checkMissingValues, length = 1)
  assertLogical(checkInObservation, length = 1)

  assertClass(cohort, class = c("cohort_table", "cdm_table"), all = TRUE, call = call)

  if(is.na(tableName(cohort))){
    missingCohortTableNameError(cdmReference(cohort), validation = validation)
  }

  # columns
  notPresent <- cohortColumns("cohort")[!cohortColumns("cohort") %in% colnames(cohort)]
  if (length(notPresent) > 0) {
    if (validation == "error") {
      cli::cli_abort(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    } else if (validation == "warning") {
      cli::cli_warn(c("!" = "columns: {.var {notPresent}} not present in cohort object"), call = call)
    }
  }
  cohort <- cohort |> dplyr::relocate(dplyr::any_of(cohortColumns("cohort")))

  if (isTRUE(checkEndAfterStart)) {
    cohort <- checkStartEnd(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkOverlappingEntries)) {
    cohort <- checkOverlap(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkMissingValues)) {
    cohort <- checkNaCohort(cohort = cohort, validation = validation, call = call)
  }
  if (isTRUE(checkInObservation)) {
    cohort <- checkObservationPeriod(cohort = cohort, validation = validation, call = call)
  }
  return(cohort)
}

#' Validate cohortId argument.
#'
#' @param cohortId A cohortId vector to be validated.
#' @param cohort A cohort_table object.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateCohortIdArgument <- function(cohortId,
                                     cohort,
                                     validation = "error",
                                     call = parent.frame()) {
  assertValidation(validation)
  assertNumeric(cohortId, integerish = TRUE, null = TRUE, min = 1, unique = TRUE, call = call)
  assertClass(cohort, class = "cohort_table", call = call)
  possibleCohortIds <- settings(cohort) |>
    dplyr::pull("cohort_definition_id") |>
    as.integer()
  if (is.null(cohortId)) {
    cohortId <- possibleCohortIds
  } else {
    cohortId <- as.integer(cohortId)
    notPresent <- cohortId[!cohortId %in% possibleCohortIds]
    cohortId <- cohortId[cohortId %in% possibleCohortIds]
    if (length(notPresent) > 0) {
      if (validation == "error" | length(cohortId) == 0) {
        cli::cli_abort("cohort definition id: {notPresent} not defined in settings.", call = call)
      } else if (validation == "warning") {
        cli::cli_warn(c("!" = "cohort definition id: {notPresent} not considered as they are not defined in settings."), call = call)
      }
    }
  }
  return(cohortId)
}

#' Validate conceptSet argument.
#'
#' @param conceptSet It can be either a named list of concepts or a codelist,
#' codelist_with_details or conceptSetExpression object.
#' @param cdm A cdm_reference object, needed if a conceptSetExpression is
#' provided.
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @export
#'
validateConceptSetArgument <- function(conceptSet,
                                       cdm = NULL,
                                       validation = "error",
                                       call = parent.frame()) {
  if (inherits(conceptSet, "codelist")) {
    conceptSet <- validateCodelist(conceptSet, call = call)
  } else if (inherits(conceptSet, "codelist_with_details")) {
    conceptSet <- validateCodelistWithDetails(conceptSet, call) |>
      lapply(dplyr::pull, "concept_id")
  } else if (inherits(conceptSet, "conceptSetExpression")) {
    concepts <- validateConceptSetExpression(conceptSet, call)
    concepts <- concepts |>
      lapply(dplyr::select, c("concept_id", "excluded", "decendants")) |>
      dplyr::bind_rows(.id = "concept_name")
    descendants <- concepts |>
      dplyr::filter(.data$decendants == TRUE) |>
      dplyr::select(-"decendants")
    if (nrow(descendants) > 0) {
      assertClass(cdm, "cdm_reference")
      nm <- uniqueTableName()
      cdm <- insertTable(cdm = cdm, name = nm, table = descendants)
      descendants <- cdm$concept_ancestor |>
        dplyr::rename("to_join" = "ancestor_concept_id") |>
        dplyr::inner_join(
          cdm[[nm]] |>
            dplyr::rename("to_join" = "concept_id"),
          by = "concept_id"
        ) |>
        dplyr::select(
          "concept_id" = "descendant_concept_id", "excluded", "concept_name"
        ) |>
        dplyr::collect()
      concepts <- concepts |>
        dplyr::filter(.data$descendants == FALSE) |>
        dplyr::select(-"decendants") |>
        dplyr::union_all(descendants)
    } else {
      concepts <- concepts |> dplyr::select(-"decendants")
    }
    conceptSet <- concepts |>
      dplyr::filter(.data$excluded == FALSE) |>
      dplyr::select(-"excluded") |>
      dplyr::anti_join(
        concepts |>
          dplyr::filter(.data$excluded == TRUE),
        by = c("concept_id", "concept_name")
      ) |>
      dplyr::group_by(.data$concept_name) |>
      dplyr::group_split() |>
      as.list() |>
      lapply(function(x) {
        nm <- unique(x$concept_name)
        list(unique(x$concept_id)) |> rlang::set_names(nm)
      }) |>
      unlist(recursive = F)
  } else {
    conceptSet <- newCodelist(conceptSet)
  }
  return(conceptSet)
}

assertValidation <- function(validation, call = parent.frame()) {
  validation |>
    assertChoice(choices = c("error", "warning"), length = 1, call = call)
}

#' validateWindowArgument
#'
#' @param window time window
#' @param snakeCase return default window  name in snake case if TRUE
#' @param call A call argument to pass to cli functions.
#'
#' @return time window
#' @export
#'
validateWindowArgument <- function(window,
                                   snakeCase = TRUE,
                                   call = parent.frame()) {


  assertLogical(snakeCase, length = 1,call = call)

  if (!is.list(window)) {
    window <- list(window)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please use Inf or -Inf instead", call = call)
  }

  originalWindow <- window
  # change inf to NA to check for floats, as Inf won't pass integerish check
  window <-
    lapply(window, function(x)
      replace(x, is.infinite(x), NA))
  assertList(window, class = "numeric", call = call)
  assertNumeric(
    window |> unlist(),
    integerish = TRUE,
    na = TRUE,
    call = call
  )
  window <- originalWindow

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    cli::cli_abort("window can only contain two values: windowStart and windowEnd",
                   call = call)
  }

  # eg if list(1,2,3), change to list(c(1,1), c(2,2), c(3,3))
  if (length(window) > 1 && any(lengths(window) == 1)) {
    window[lengths(window) == 1] <- lapply(window[lengths(window) == 1],
                                           function(x)
                                             c(unlist(x[lengths(x) == 1]),
                                               unlist(x[lengths(x) == 1])))
    cli::cli_warn(
      "Window list contains element with only 1 value provided,
          use it as both window start and window end"
    )
  }

  assertWindowName(window,snakeCase, call = call)



}

#' @noRd
getWindowNames <- function(window, snakeCase) {
  #snakecase
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- stringr::str_replace_all(string = element,
                                        pattern = "-",
                                        replacement = "m")
    invisible(paste0(element[1], "_to_", element[2]))
  }
  #snakecase False
  getname2 <- function(element) {
    element <- tolower(as.character(element))
    invisible(paste0(element[1], " to ", element[2]))
  }

  windowNames <- names(window)

  if (isTRUE(snakeCase)) {
    if (is.null(windowNames)) {
      windowNames <- lapply(window, getname)
    } else {
      windowNames[windowNames == ""] <-
        lapply(window[windowNames == ""], getname)
    }
  } else {
    if (is.null(windowNames)) {
      windowNames <- lapply(window, getname2)
    } else {
      windowNames[windowNames == ""] <-
        lapply(window[windowNames == ""], getname2)
    }
  }
  invisible(windowNames)
}

#' @noRd
assertWindowName <-
  function(window, snakeCase, call = parent.frame()) {
    names(window) <- getWindowNames(window, snakeCase = snakeCase)
    lower <- lapply(window, function(x) {
      x[1]
    }) |> unlist()
    upper <- lapply(window, function(x) {
      x[2]
    }) |> unlist()

    if (any(lower > upper)) {
      cli::cli_abort("First element in window must be smaller or equal to
                   the second one",
                     call = call)
    }
    if (any(is.infinite(lower) & lower == upper & sign(upper) == 1)) {
      cli::cli_abort("Not both elements in the window can be +Inf", call = call)
    }
    if (any(is.infinite(lower) &
            lower == upper & sign(upper) == -1)) {
      cli::cli_abort("Not both elements in the window can be -Inf", call = call)
    }

    invisible(window)
  }

#' validateAgeGroupArgument
#'
#' @param ageGroup age group in a list.
#' @param multipleAgeGroup allow mutliple age group.
#' @param overlap allow overlapping ageGroup.
#' @param null null age group allowed true or false.
#' @param ageGroupName Name of the default age group.
#' @param call parent frame.
#'
#' @return validate ageGroup
#' @export
#'
validateAgeGroupArgument <- function(ageGroup,
                                     multipleAgeGroup = TRUE,
                                     overlap = FALSE,
                                     null = TRUE,
                                     ageGroupName = "age_group",
                                     call = parent.frame()) {
  # initial checks
  assertLogical(null, length = 1)
  assertLogical(multipleAgeGroup, length = 1)
  assertLogical(overlap, length = 1)
  assertCharacter(ageGroupName, length = 1)

  if (is.null(ageGroup)) {
    if (null) {
      return(invisible(NULL))
    } else {
      cli::cli_abort("`ageGroup` argument can not be NULL.", call = call)
    }
  }

  # convert to list of lists if it is not
  if (is.numeric(ageGroup)) {
    ageGroup <- list(list(ageGroup))
  } else if (rlang::is_bare_list(ageGroup)) {
    if (length(ageGroup) == 0) {
      return(invisible(NULL))
    } else if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list(ageGroup)
    }
  } else {
    cli::cli_abort("`ageGroup` must be a list of age groups.", call = call)
  }

  len <- length(ageGroup)

  #check multiple age group
  if (!multipleAgeGroup & len > 1) {
    cli::cli_abort("Multiple age group are not allowed", call = call)
  }

  # correct individual age groups
  ageGroup <- ageGroup |>
    purrr::map(\(x) correctAgeGroup(x, overlap = overlap, call = call))

  # correct age group names
  nms <- names(ageGroup)
  if (is.null(nms)) nms <- rep("", len)
  if (len == 1 & identical(nms, "")) {
    names(ageGroup) <- ageGroupName
  } else {
    for (k in seq_len(len)) {
      if (nms[k] == "") nms[k] <- paste0(ageGroupName, "_", k)
    }
    names(ageGroup) <- nms
  }

  invisible(ageGroup)
}

# check they are integer, positive, min<=max
correctAgeGroup <- function(ageGroup,
                            overlap,
                            call) {
  len <- length(ageGroup)

  # assert numeric
  isNumeric <- purrr::map_lgl(ageGroup, is.numeric) |>
    all()
  if (!isNumeric) {
    "Elements of `ageGroup` argument are not numeric." |>
      cli::cli_abort(call = call)
  }

  # correct length 1
  ageGroup <- purrr::map(ageGroup, \(x) {
    if (length(x) == 1) rep(x, 2) else x
  })

  # length 2
  if (any(lengths(ageGroup) != 2)) {
    "Elements of `ageGroup` must have length 2." |>
      cli::cli_abort(call = call)
  }

  allValues <- unlist(ageGroup)

  # no NA
  if (any(is.na(allValues))) {
    "Elements of `ageGroup` argument can not contain NA." |>
      cli::cli_abort(call = call)
  }

  # assert integerish
  if (!isIntegerish(allValues)) {
    "Elements of `ageGroup` argument must be integerish." |>
      cli::cli_abort(call = call)
  }

  # convert to numeric as Inf can not be integer
  ageGroup <- purrr::map(ageGroup, as.numeric)

  # positive
  if (any(unlist(ageGroup) < 0L)) {
    "Elements of `ageGroup` argument must be greater or equal to {.val 0}." |>
      cli::cli_abort(call = call)
  }

  # min <= max
  isMinBigger <- purrr::map_lgl(ageGroup, \(x) x[1] > x[2]) |>
    any()
  if (isMinBigger) {
    "First element of `ageGroup` argument must be smaller or equal than the second one." |>
      cli::cli_abort(call = call)
  }

  # overlap
  if (!overlap & len > 1) {
    for (i in 1:(len-1)) {
      for (j in (i+1):len) {
        if (thereIsOverlap(ageGroup[[i]], ageGroup[[j]])) {
          "`ageGroup` must not contain overlap between groups." |>
            cli::cli_abort(call = call)
        }
      }
    }
  }

  # add names
  if (is.null(names(ageGroup))) {
    nms <- rep("", len)
  } else {
    nms <- names(ageGroup)
  }
  for (k in seq_len(len)) {
    if (nms[k] == "") nms[k] <- nameAgeGroup(ageGroup[[k]])
  }
  names(ageGroup) <- nms

  return(ageGroup)
}
isIntegerish <- function(x) {
  if (is.integer(x)) return(TRUE)
  xInt <- x[!is.infinite(x)]
  err <- max(abs(xInt - round(xInt)))
  err < 0.0001
}
thereIsOverlap <- function(x, y) {
  if (x[1] < y[1] & x[2] < y[1]) return(FALSE)
  if (y[1] < x[1] & y[2] < x[1]) return(FALSE)
  TRUE
}
nameAgeGroup <- function(x) {
  if (x[1] == 0L & is.infinite(x[2])) return("overall")
  if (is.infinite(x[2])) return(paste(x[1], "or above"))
  paste(x[1], "to", x[2])
}

#' validateCdmArgument
#'
#' @param cdm A cdm_reference object
#' @param checkOverlapObservation TRUE to perform check on no overlap
#' observation period
#' @param checkStartBeforeEndObservation TRUE to perform check on correct
#' observational start and end date
#' @param checkPlausibleObservationDates TRUE to perform check that there are
#' no implausible observation period start dates (before 1800-01-01) or end
#' dates (after the current date)
#' @param checkPerson TRUE to perform check on person id in all clincial table
#' are in person table
#' @param validation How to perform validation: "error", "warning".
#' @param call A call argument to pass to cli functions.
#'
#' @return A cdm_reference object
#' @export
#'
validateCdmArgument <- function(cdm,
                                checkOverlapObservation = FALSE,
                                checkStartBeforeEndObservation = FALSE,
                                checkPlausibleObservationDates = FALSE,
                                checkPerson = FALSE,
                                validation = "error",
                                call = parent.frame()) {
  assertValidation(validation, call = call)
  assertLogical(checkOverlapObservation,
                length = 1,
                call = call)
  assertLogical(checkStartBeforeEndObservation,
                length = 1,
                call = call)


  # validate
  # assert class
  assertClass(cdm,
              class = c("cdm_reference"),
              all = TRUE,
              call = call)

  # not overlapping periods
  if (isTRUE(checkOverlapObservation)){
    checkOverlapObservation(cdm$observation_period)
  }

  # no start observation before end
  if (isTRUE(checkStartBeforeEndObservation)){
    checkStartBeforeEndObservation(cdm$observation_period)
  }

  if (isTRUE(checkPlausibleObservationDates)){
    checkPlausibleObservationDates(cdm$observation_period)

  }

  if (isTRUE(checkPerson)){
    checkPerson(cdm = cdm, call = call)
  }

  return(invisible(cdm))
}

#' validateResultArgument
#'
#' @param result summarise result object to validate
#' @param validation message to return
#' @param call parent.frame
#'
#' @return summarise result object
#' @export
#'
validateResultArgument <- function(result,
                                   validation = "error",
                                   call = parent.frame()) {
  assertValidation(validation, call = call)
  assertTable(result, call = call)

  result <- validateSummariseResult(result)

  return(invisible(result))
}

#' validateResultArgument
#'
#' @param result summarise result object to validate
#' @param validation message to return
#' @param call parent.frame
#'
#' @return summarise result object
#' @export
#'
validateResultArguemnt <- function(result,
                                   validation = "error",
                                   call = parent.frame()) {
  lifecycle::deprecate_soft(
    "0.4.0", "validateResultArguemnt()", "validateResultArgument()"
  )
  validateResultArgument(result = result, validation = validation, call = call)
}
