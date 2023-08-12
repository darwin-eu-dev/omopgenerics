# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
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

#' It creates a mock cdm_reference.
#'
#' @param cdmVocabulary A cdm_reference that contains the vocabulary tables.
#' @param cdmName Name of the cdm.
#' @param individuals Number of individuals in the mock database or table with
#' the demographics data.
#' @param person Person table.
#' @param observation_period Observation period table.
#' @param death death table.
#' @param condition_occurrence Condition occurrence table.
#' @param drug_exposure DrugExposure table.
#' @param procedure_occurrence Procedure occurrence table.
#' @param device_exposure Device exposure table.
#' @param measurement Measurement table.
#' @param observation Observation table.
#' @param numberRecords number of records per person.
#' @param seed Random seed.
#' @param ... Tables to be added to the cdm. If cohorts the attributes will be
#' updated.
#'
#' @return A cdm reference with the mock tables
#'
#' @export
#'
mockCdm <- function(cdmVocabulary = mockVocabularyCdm(),
                    cdmName = "MOCK CDM",
                    individuals = 10,
                    person = NULL,
                    observation_period = NULL,
                    death = NULL,
                    condition_occurrence = NULL,
                    drug_exposure = NULL,
                    procedure_occurrence = NULL,
                    device_exposure = NULL,
                    measurement = NULL,
                    observation = NULL,
                    numberRecords = c(default = 2),
                    seed = 1,
                    ...) {
  # check inputs
  checkInput(
    cdmVocabulary = cdmVocabulary, cdmName = cdmName, individuals = individuals,
    person = person, observation_period = observation_period, death = death,
    condition_occurrence = condition_occurrence, drug_exposure = drug_exposure,
    procedure_occurrence = procedure_occurrence,
    device_exposure = device_exposure, measurement = measurement,
    observation = observation, seed = seed, cdmTables = list(...)
  )

  cdmTables <- list(
    person = person, observation_period = observation_period, death = death,
    condition_occurrence = condition_occurrence,
    drug_exposure = drug_exposure, measurement = measurement,
    procedure_occurrence = procedure_occurrence, observation = observation,
    device_exposure = device_exposure, ...
  )

  cdmTables <- c(cdmVocabulary, cdmTables)
  cdm <- newCdmReference(
    cdmTables = cdmTables, cdmName = cdmName, cdmVersion = attr()
  )

#   cdm <- generatePerson(cdm = cdm, individuals = individuals, seed = seed)
#   cdm <- generateObservationPeriod(
#     cdm = cdm, observationPeriodPerPerson = 1, seed =seed
#   )
#   cdm <- generateDeath(cdm = cdm, deathFraction = 0.3, seed = seed)
#   cdm <- generateConditionOccurrence(
#     cdm = cdm, conditionOccurrencePerPerson = 3, seed = seed
#   )
#   cdm <- generateDrugExposure(
#     cdm = cdm, drugExposurePerPerson = 5, seed = seed
#   )
#   cdm <- generateMeasurement(
#     cdm = cdm, measurementPerPerson = 2, seed = seed
#   )
#   cdm <- generateProcedureOccurrence(
#     cdm = cdm, procedureOccurrencePerPerson = 1, seed = seed
#   )
#   cdm <- generateObservation(
#     cdm = cdm, observationPerPerson = 1, seed = seed
#   )
#   cdm <- generateDeviceExposure(
#     cdm = cdm, deviceExposurePerPerson = 1, seed = seed
#   )
#   cdm <- generateDrugEra(cdm = cdm)
#   cdm <- generateConditionEra(cdm = cdm)
#   cdm <- generateCohorts(cdm = cdm, numberCohorts = 2, seed = seed)
#
#  return(cdm)
}


#' To add the cohort set if NULL
#' @noRd
createCohortSet <- function(cohort) {
  cohort %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate("cohort_name" = paste0(
      "cohort_",
      .data$cohort_definition_id
    ))
}

#' To add the cohort count if NULL
#' @noRd
createCohortCount <- function(cohort) {
  cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    )
}

#' To add the cohort attrition if NULL
#' @noRd
createCohortAttrition <- function(cohort) {
  cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      reason_id = 1, reason = "Qualifying initial records",
      excluded_records = 0, excluded_subjects = 0
    )
}

#' To create the person table
#' @noRd
generatePersonObservationPeriod <- function(cdm, individuals, seed) {
  if (is.null(cdm[["person"]])) {
    if (is.numeric(individuals)) {
      individuals <- dplyr::tibble(
        sex = sample(c("Male", "Female"), individuals, replace = TRUE),
        year_birth = sample(1950:2023, individuals, replace = TRUE)
      ) %>%
        dplyr::mutate(observation_start = .data$year_birth + sample(
          1:50, individuals, replace = TRUE
        )) %>%
        dplyr::mutate(observation_end = .data$observation_start + sample(
          1:50, individuals, replace = TRUE
        )) %>%
        dplyr::mutate(

        )
    }
    # set.seed(seed)
    # person <- pseudoPersonObsTable(individuals)
    # cdm[["person"]] <- person %>%
    #   dplyr::mutate(
    #     person_id = dplyr::row_number(),
    #     gender_concept_id = dplyr::if_else(.data$sex == "Male", 8507, 8532),
    #     year_of_birth = .data$year_birth,
    #     month_of_bith = 1,
    #     day_of_birth = 1,
    #     birth_datetime = as.Date(paste0(.data$year_birth, "-01-01"))
    #   )
  } else {
    cdm[["person"]] <- correctTable(cdm[["person"]], "person")
  }
  return(cdm)
}

#' #' To create the observation period tables
#' #' @noRd
#' createObservationPeriod <- function(person) {
#'   person %>%
#'     dplyr::select("person_id", "birth_datetime") %>%
#'     dplyr::mutate(upper_limit = as.Date("2023-01-01")) %>%
#'     createDate(
#'       "observation_period_start_date", "birth_datetime", "upper_limit"
#'     ) %>%
#'     createDate(
#'       "observation_period_end_date", "observation_period_start_date",
#'       "upper_limit"
#'     ) %>%
#'     dplyr::mutate(
#'       observation_period_id = dplyr::row_number(),
#'       period_type_concept_id = 44814724
#'     ) %>%
#'     dplyr::select(
#'       "observation_period_id", "person_id", "observation_period_start_date",
#'       "observation_period_end_date", "period_type_concept_id"
#'     )
#' }
#'
#' #' To add the attributes to the cohorts
#' #' @noRd
#' addCohortAttributes <- function(cohort) {
#'   if (is.null(attr(cohort, "cohort_set"))) {
#'     cohort <- addCohortSet(cohort)
#'   }
#'   if (is.null(attr(cohort, "cohort_count"))) {
#'     cohort <- addCohortCount(cohort)
#'   }
#'   if (is.null(attr(cohort, "cohort_attrition"))) {
#'     cohort <- addCohortAttrition(cohort)
#'   }
#'   return(cohort)
#' }
#'
#' #' To create the cohorts or add the attributes to the existing ones
#' #' @noRd
#' createCohorts <- function(cohorts, observation_period) {
#'   if (!("cohort1" %in% names(cohorts))) {
#'     cohorts[["cohort1"]] <- createCohort(observation_period)
#'   }
#'   if (!("cohort2" %in% names(cohorts))) {
#'     cohorts[["cohort2"]] <- createCohort(observation_period)
#'   }
#'   for (name in names(cohorts)) {
#'     cohorts[[name]] <- addCohortAttributes(cohorts[[name]])
#'   }
#'   return(cohorts)
#' }
#'
#' #' To create a random cohort from observation period
#' #' @noRd
#' createCohort <- function(observation_period) {
#'   cohort <- observation_period %>%
#'     dplyr::group_by(.data$person_id) %>%
#'     dplyr::filter(dplyr::row_number() == 1) %>%
#'     dplyr::ungroup() %>%
#'     createDate(
#'       "cohort_start_date", "observation_period_start_date",
#'       "observation_period_end_date"
#'     ) %>%
#'     createDate(
#'       "cohort_end_date", "cohort_start_date", "observation_period_end_date"
#'     )
#'   cohort <- cohort %>%
#'     dplyr::mutate(
#'       cohort_definition_id = sample(1:3, nrow(cohort), replace = T)
#'     ) %>%
#'     dplyr::select(
#'       "cohort_definition_id", "subject_id" = "person_id", "cohort_start_date",
#'       "cohort_end_date"
#'     )
#' }
#'
#' #' To create a mock drug_exposure table
#' #' @noRd
#' createDrugExposure <- function(observation_period, concept) {
#'   concepts <- concept %>%
#'     dplyr::filter(.data$domain_id == "Drug") %>%
#'     dplyr::filter(.data$concept_class_id != "Ingredient") %>%
#'     dplyr::pull("concept_id")
#'   if (length(concepts) > 0) {
#'     drug_exposure <- observation_period %>%
#'       dplyr::mutate(number_records = stats::rpois(dplyr::n(), 3)) %>%
#'       tidyr::uncount(.data$number_records) %>%
#'       createDate(
#'         "drug_exposure_start_date", "observation_period_start_date",
#'         "observation_period_end_date"
#'       ) %>%
#'       createDate(
#'         "drug_exposure_end_date", "drug_exposure_start_date",
#'         "observation_period_end_date"
#'       ) %>%
#'       dplyr::mutate(
#'         drug_exposure_id = dplyr::row_number(),
#'         drug_type_concept_id = 38000177
#'       )
#'     drug_exposure <- drug_exposure %>%
#'       dplyr::mutate(
#'         drug_concept_id = sample(concepts, nrow(drug_exposure), replace = T),
#'         quantity = sample(
#'           c(1, seq(5, 50, 5), seq(60, 100, 10)), nrow(drug_exposure),
#'           replace = T
#'         )
#'       ) %>%
#'       dplyr::select(
#'         "drug_exposure_id", "person_id", "drug_concept_id",
#'         "drug_exposure_start_date", "drug_exposure_end_date",
#'         "drug_type_concept_id", "quantity"
#'       )
#'   } else {
#'     drug_exposure <- dplyr::tibble(
#'       drug_exposure_id = numeric(), person_id = numeric(),
#'       drug_concept_id = numeric(),
#'       drug_exposure_start_date = as.Date(x = integer(0), origin = "1970-01-01"),
#'       drug_exposure_end_date = as.Date(x = integer(0), origin = "1970-01-01"),
#'       drug_type_concept_id = numeric(), quantity = numeric()
#'     )
#'   }
#'   return(drug_exposure)
#' }
#'
#' #' To create a condition_occurrence table based on observation_period
#' #' @noRd
#' createConditionOccurrence <- function(observation_period, concept) {
#'   concepts <- concept %>%
#'     dplyr::filter(.data$domain_id == "Condition") %>%
#'     dplyr::pull("concept_id")
#'   if (length(concepts) > 0) {
#'     condition_occurrence <- observation_period %>%
#'       dplyr::mutate(number_records = stats::rpois(dplyr::n(), 2)) %>%
#'       tidyr::uncount(.data$number_records) %>%
#'       createDate(
#'         "condition_start_date", "observation_period_start_date",
#'         "observation_period_end_date"
#'       ) %>%
#'       createDate(
#'         "condition_end_date", "condition_start_date",
#'         "observation_period_end_date"
#'       ) %>%
#'       dplyr::mutate(
#'         condition_occurrence_id = dplyr::row_number(),
#'         condition_type_concept_id = 32020
#'       )
#'     condition_occurrence <- condition_occurrence %>%
#'       dplyr::mutate(condition_concept_id = sample(
#'         concepts, nrow(condition_occurrence), replace = T
#'       )) %>%
#'       dplyr::select(
#'         "condition_occurrence_id", "person_id", "condition_concept_id",
#'         "condition_start_date", "condition_end_date",
#'         "condition_type_concept_id"
#'       )
#'   } else {
#'     condition_occurrence <- dplyr::tibble(
#'       condition_occurrence_id = numeric(), person_id = numeric(),
#'       condition_concept_id = numeric(),
#'       condition_start_date = as.Date(x = integer(0), origin = "1970-01-01"),
#'       condition_end_date = as.Date(x = integer(0), origin = "1970-01-01"),
#'       condition_type_concept_id = numeric()
#'     )
#'   }
#'   return(condition_occurrence)
#' }
#'
#' #' To create visit occurrence from condition_occurrence and drug_exposure
#' #' @noRd
#' createVisitOccurrence <- function(condition_occurrence, drug_exposure) {
#'   condition_occurrence %>%
#'     dplyr::select(
#'       "person_id", "visit_start_date" = "condition_start_date",
#'       "visit_end_date" = "condition_end_date"
#'     ) %>%
#'     dplyr::union_all(
#'       drug_exposure %>%
#'         dplyr::select(
#'           "person_id", "visit_start_date" = "drug_exposure_start_date",
#'           "visit_end_date" = "drug_exposure_end_date"
#'         )
#'     ) %>%
#'     dplyr::mutate(
#'       visit_occurrence_id = dplyr::row_number(),
#'       visit_concept_id = 9202
#'     ) %>%
#'     dplyr::select(
#'       "visit_occurrence_id", "person_id", "visit_concept_id",
#'       "visit_start_date", "visit_end_date"
#'     )
#' }
#'
#' #' To create observation table based on observation_period
#' #' @noRd
#' createObservation <- function(observation_period, concept) {
#'   concepts <- concept %>%
#'     dplyr::filter(.data$domain_id == "Observation") %>%
#'     dplyr::pull("concept_id")
#'   if (length(concepts) > 0) {
#'     observation <- observation_period %>%
#'       dplyr::mutate(number_records = stats::rpois(dplyr::n(), 2)) %>%
#'       tidyr::uncount(.data$number_records) %>%
#'       createDate(
#'         "observation_date", "observation_period_start_date",
#'         "observation_period_end_date"
#'       ) %>%
#'       dplyr::mutate(
#'         observation_id = dplyr::row_number(),
#'         observation_type_concept_id = 32020
#'       )
#'     observation <- observation %>%
#'       dplyr::mutate(observation_concept_id = sample(
#'         concepts, nrow(observation), replace = T
#'       )) %>%
#'       dplyr::select(
#'         "observation_id", "person_id", "observation_concept_id",
#'         "observation_date", "observation_type_concept_id"
#'       )
#'   } else {
#'     observation <- dplyr::tibble(
#'       observation_id = numeric(), person_id = numeric(),
#'       observation_concept_id = numeric(),
#'       observation_date = as.Date(x = integer(0), origin = "1970-01-01"),
#'       observation_type_concept_id = numeric()
#'     )
#'   }
#'   return(observation)
#' }
#'
#' generateDate <- function(.data, lower, upper, columns) {
#'   numberDates <- length(columns)
#'   lower <- as.numeric(lower)
#'   upper <- as.numeric(upper)
#'   x <- dplyr::tibble(lower = lower, upper = upper)
#'   for (k in seq_len(numberDates + 1)) {
#'     x <- x %>%
#'       dplyr::mutate(!!paste0("date", k) := stats::runif(length(lower)))
#'   }
#'   for (k in rev(seq_len(numberDates + 1))) {
#'     x <- x %>%
#'       dplyr::mutate(!!paste0("date", k) := purrr::reduce(
#'         x %>% dplyr::select(dplyr::all_of(paste0("date", seq_len(k)))), `+`
#'       ))
#'   }
#'   x <- x %>%
#'     dplyr::mutate(dplyr::across(
#'       dplyr::all_of(paste0("date", seq_len(numberDates))),
#'         ~ as.Date(.data$lower + round(
#'           (.data$upper - .data$lower) * .x / .data[[paste0(
#'             "date", numberDates + 1
#'           )]]
#'         ), origin = "1970-01-01")
#'       )
#'     ) %>%
#'     dplyr::select(-dplyr::all_of(c(
#'       "lower", "upper", paste0("date", numberDates + 1)
#'     ))) %>%
#'     as.list()
#'   for (k in seq_along(columns)) {
#'     .data <- .data %>% dplyr::mutate(!!columns[k] := x[[k]])
#'   }
#'   return(.data)
#' }
#'
#' groupYears <- function(x) {
#'   n <- table(factor(x, levels = min(x):max(x)))
#'   i <- as.numeric(names(n))
#'   n <- as.numeric(n)
#'   i <- c(NA, i, NA)
#'   n <- c(0, n, 0)
#'   summaryX <- dplyr::tibble(start = numeric(), end = numeric())
#'   for (k in seq_len(max(n))) {
#'     nk <- as.numeric(n >= k)
#'     start <- i[which(nk == 1 & dplyr::lag(nk, 1) == 0)]
#'     end <- i[which(nk == 1 & dplyr::lead(nk, 1) == 0)]
#'     summaryX <- summaryX %>%
#'       dplyr::union_all(dplyr::tibble(start = start, end = end))
#'   }
#'   return(summaryX)
#' }
#' pseudoPersonObsTable <- function(individuals) {
#'   persons <- dplyr::tibble(
#'     sex = character(), year_birth = numeric(), year_start = numeric(),
#'     year_end = numeric()
#'   )
#'   for (k in seq_len(nrow(individuals))) {
#'     sex <- individuals$sex[k]
#'     if (sex == "Both") {
#'       sex <- c("Male", "Female")
#'     }
#'     age <- individuals$age_min[k]:individuals$age_max[k]
#'     n <- individuals$number_individuals[k]
#'     persons <- persons %>%
#'       dplyr::union_all(dplyr::tibble(
#'         sex = sample(x = sex, size = n, replace = TRUE),
#'         year_birth = individuals$year_start[k] - sample(
#'           x = age, size = n, replace = TRUE
#'         ),
#'         year_start = individuals$year_start[k],
#'         year_end = individuals$year_end[k]
#'       ))
#'   }
#'   persons <- persons %>%
#'     dplyr::group_by(.data$sex, .data$year_birth) %>%
#'     dplyr::mutate(id = dplyr::cur_group_id()) %>%
#'     dplyr::ungroup()
#'   groups <- unique(persons$id)
#'   person <- dplyr::tibble(
#'     sex = character(), year_birth = numeric(), start = numeric(),
#'     end = numeric()
#'   )
#'   for (k in groups) {
#'     x <- persons %>% dplyr::filter(.data$id == .env$k)
#'     years <- lapply(seq_len(nrow(x)), function(id) {
#'       x$year_start[id]:x$year_end[id]
#'     }) %>%
#'       unlist()
#'     person <- person %>%
#'       dplyr::union_all(
#'         groupYears(years) %>%
#'           dplyr::mutate(year_birth = x$year_birth[1], sex = x$sex[1])
#'       )
#'   }
#'   return(person)
#' }
