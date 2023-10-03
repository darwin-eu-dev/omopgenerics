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
#' @param observationPeriod Observation period table.
#' @param death death table.
#' @param conditionOccurrence Condition occurrence table.
#' @param drugExposure DrugExposure table.
#' @param procedureOccurrence Procedure occurrence table.
#' @param deviceExposure Device exposure table.
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
                    individuals = NULL,
                    person = NULL,
                    observationPeriod = NULL,
                    death = NULL,
                    conditionOccurrence = NULL,
                    drugExposure = NULL,
                    procedureOccurrence = NULL,
                    deviceExposure = NULL,
                    measurement = NULL,
                    observation = NULL,
                    numberRecords = c(default = 2),
                    seed = 1,
                    ...) {
  # check inputs
  checkInput(
    cdmVocabulary = cdmVocabulary, cdmName = cdmName, individuals = individuals,
    numberRecords = numberRecords, seed = seed, cdmTables = list(
      person = person, observationPeriod = observationPeriod, death = death,
      conditionOccurrence = conditionOccurrence, drugExposure = drugExposure,
      procedureOccurrence = procedureOccurrence,
      deviceExposure = deviceExposure, measurement = measurement,
      observation = observation, ...
    )
  )

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  if (is.null(names(numberRecords))) {
    numberRecords <- c(default = numberRecords)
  }

  cdmTables <- c(
    cdmVocabulary, person = person, observation_period = observationPeriod,
    death = death, condition_occurrence = conditionOccurrence,
    drug_exposure = drugExposure, measurement = measurement,
    procedure_occurrence = procedureOccurrence, observation = observation,
    device_exposure = deviceExposure, ...
  )

  cdm <- newCdmReference(
    cdmTables = cdmTables, cdmName = cdmName, cdmVersion = attr()
  )

  cdm <- generatePersonObservationPeriod(cdm, individuals, seed)

  clinical <- c(
    "death", "condition_occurrence", "drug_exposure", "procedure_occurrence",
    "device_exposure", "measurement", "observation"
  )
  for (tab in clinical) {
    if (tab %in% names(numberRecords)) {
      records <- round(unname(numberRecords[tab]) * nrow(cdm$person))
    } else {
      records <- round(unname(numberRecords["default"]) * nrow(cdm$person))
    }
    cdm[[tab]] <- generateClinicalDataTable(cdm, tab, records)
  }

  cdm <- generateDrugEra(cdm = cdm)
  cdm <- generateConditionEra(cdm = cdm)

  cdm <- generateCohorts(cdm = cdm, cohortNames = c("cohort1", "cohort2"))

  return(cdm)
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

#' Function create the observation period in a cdm reference.
#'
#' @param cdm A cdm_reference object.
#' @param individuals A tibble with the number of individuals, sex, year of
#' birth, start observation and end observation.
#' @param seed Seed for random numbers reproducibility.
#' @param cdmVersion Verison of the cdm.
#'
#' @return A cdm_reference object
#'
#' @export
#'
generatePersonObservationPeriod <- function(cdm,
                                            individuals = NULL,
                                            seed = NULL,
                                            cdmVersion = attr(cdm, "cdm_version")) {
  # initial checks
  checkInput(
    cdm = cdm, individuals = individuals, seed = seed, cdmVersion = cdmVersion
  )

  # set initial seed
  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  if (is.null(individuals)) {
    individuals <- exisitingIndividuals(cdm)
    if (is.null(cdm$person)) {
      cdm$person <- completePersonTable(individuals)
    }
    if (is.null(cdm$observation_period)) {
      cdm$observation_period <- generateObservationPeriod(cdm, individuals)
    }
  } else {
    individuals <- correctIndividuals(individuals)
    cdm$person <- personFromTibble(individuals)
    cdm$observation_period <- observationPeriodFromCdm(cdm)
  }

  return(cdm)
}

correctIndividuals <- function(individuals) {
  if (is.numeric(individuals)) {
    individuals <- dplyr::tibble(
      person_id = seq_len(individuals), sex = as.character(NA),
      birth = as.Date(NA), start_observation = as.Date(NA),
      end_observation = as.Date(NA)
    )
  } else {
    cols <- c("sex", "birth", "start_observation", "end_observation")
    for (col in cols[!(cols %in% colnames(cols))]) {
      individuals <- individuals %>% dplyr::mutate(!!col := NA)
    }
    individuals <- individuals %>%
      dplyr::mutate(
        number_individuals = as.numeric(.data$number_individuals),
        sex = as.character(.data$sex),
        birth = as.Date(.data$birth),
        start_observation = as.Date(.data$start_observation),
        end_observation = as.Date(.data$end_observation)
      )
    individuals <- lapply(seq_len(nrow(individuals)), function (x) {
      dplyr::tibble(
        person_id = seq_len(individuals$number_individuals[x]),
        sex = individuals$sex[x], birth = individuals$birth[x],
        start_observation = individuals$start_observation[x],
        end_observation = individuals$end_observation[x]
      )
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(person_id = dplyr::row_number())
  }
  return(individuals)
}
exisitingIndividuals <- function(cdm) {
  tables <- c(
    "observation_period", "death", "condition_occurrence", "drug_exposure",
    "procedure_occurrence", "device_exposure", "measurement", "observation"
  )
  individuals <- list()
  for (tab in tables) {
    table <- cdm[[tab]]
    if (!is.null(table)) {
      dates <- fieldsTables %>%
        dplyr::filter(.data$cdmTableName == .env$tab) %>%
        dplyr::filter(.data$cdmDatatype == "date") %>%
        dplyr::pull("cdmFieldName")
      individuals[[tab]] <- dplyr::tibble(
        person_id = table$person_id,
        min_date = purrr::reduce(
          table %>% dplyr::select(dplyr::any_of(dates)), min
        ),
        max_date = purrr::reduce(
          table %>% dplyr::select(dplyr::any_of(dates)), max
        )
      )
    }
  }
  individuals <- dplyr::bind_rows(individuals) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::summarise(
      min_date = min(.data$date), max_date = max(.data$date), .groups = "drop"
    )
  return(individuals)
}
completePersonTable <- function(individuals) {
  n <- nrow(individuals)
  cols <- fieldsTables %>%
    dplyr::filter(.data$cdmTableName == "person") %>%
    dplyr::pull("cdmFieldName")
  individuals %>%
    dplyr::mutate(
      gender_concept_id = sample(c(8532, 8507), size = n, replace = TRUE),
      birth_datetime = .data$min_date - round(60*365*runif(n)),
      race_concept_id = 0,
      ethnicity_concept_id = 0,
      location_id = as.numeric(NA),
      provider_id = as.numeric(NA),
      care_site_ide = as.numeric(NA),
      person_source_value = as.character(NA),
      gender_source_value = as.character(NA),
      gender_source_concept_id = as.character(NA),
      race_source_value = as.character(NA),
      race_source_concept_id = as.numeric(NA),
      ethnicity_source_value = as.character(NA),
      ethnicity_source_concept_id = as.numeric(NA)
    ) %>%
    dplyr::mutate(
      year_of_birth = lubridate::year(.data$birth_datetime),
      month_of_birth = lubridate::month(.data$birth_datetime),
      day_of_birth = lubridate::day(.data$birth_datetime)
    ) %>%
    dplyr::select(dplyr::any_of(cols))
}
personFromTibble <- function(individuals) {
  n <- sum(individuals$number_individuals)
  cols <- fieldsTables %>%
    dplyr::filter(.data$cdmTableName == "person") %>%
    dplyr::pull("cdmFieldName")
  individuals %>%
    dplyr::mutate(
      person_id = dplyr::row_number(),
      gender_concept_id = dplyr::if_else(
        .data$sex == "Male", 8507, dplyr::if_else(
          .data$sex == "Female", 8532, sample(c(8507, 8532), n, TRUE)
        )
      ),
      birth_datetime = dplyr::if_else(
        is.na(.data$birth),
        as.Date(round(50*runif(n)), origin = "1970-01-01"),
        as.Date(.data$birth)
      ),
      race_concept_id = 0,
      ethnicity_concept_id = 0,
      location_id = as.numeric(NA),
      provider_id = as.numeric(NA),
      care_site_ide = as.numeric(NA),
      person_source_value = as.character(NA),
      gender_source_value = as.character(NA),
      gender_source_concept_id = as.character(NA),
      race_source_value = as.character(NA),
      race_source_concept_id = as.numeric(NA),
      ethnicity_source_value = as.character(NA),
      ethnicity_source_concept_id = as.numeric(NA)
    ) %>%
    dplyr::mutate(
      year_of_birth = lubridate::year(.data$birth_datetime),
      month_of_birth = lubridate::month(.data$birth_datetime),
      day_of_birth = lubridate::day(.data$birth_datetime)
    ) %>%
    dplyr::select(dplyr::any_of(cols))
}
generateObservationPeriod <- function(cdm, individuals) {
  n <- nrow(individuals)
  cols <- fieldsTables %>%
    dplyr::filter(.data$cdmTableName == "observation_period") %>%
    dplyr::pull("cdmFieldName")
  individuals <- individuals %>%
    dplyr::inner_join(
      cdm$person %>%
        dplyr::select("person_id", "birth_datetime"),
      by = "person_id"
    ) %>%
    dplyr::mutate(
      observation_period_start_date = .data$brth_datetime + round(
        runif(n)*as.numeric(.data$min_date - .data$brth_datetime)
      ),
      observation_period_id = dplyr::row_number(),
      period_type_concept_id  = 0
    )
  if (is.null(cdm$death)) {
    individuals <- individuals %>%
      dplyr::mutate(
        observation_period_end_date = .data$max_date + 5*365*runif(n)
      )
  } else {
    individuals <- individuals %>%
      dplyr::mutate(observation_period_end_date = .data$max_date)
  }
  individuals <- individuals %>%
    dplyr::select(dplyr::all_of(cols))
  return(individuals)
}
observationPeriodFromCdm <- function(cdm, individuals) {
  n <- nrow(cdm$person)
  individuals <- cdm$person %>%
    dplyr::mutate(
      min_date = .data$birth_datetime + round(60*365*runif(n)),
      max_date = .data$min_date + round(60*365*runif(n))
    )
  generateObservationPeriod(cdm, individuals)
}

#' Function to add a clinical data table in a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#' @param tableName Name of the table to be created.
#' @param numberRecords Number of records in the table.
#' @param seed Seed for random numbers reproducibility.
#' @param cdmVersion Verison of the cdm.
#'
#' @return A cdm_reference object
#'
#' @export
#'
generateClinicalDataTable <- function(cdm,
                                      tableName,
                                      numberRows,
                                      seed = NULL,
                                      cdmVersion = attr(cdm, "cdm_version")) {
  # initial checks
  checkInput(
    cdm = cdm, clinicalTableName = clinicalTableName, numberRows = numberRows, seed = seed,
    cdmVersion = cdmVersion,
    .options = list(cdmRequiredTables = c("person", "observation_period"))
  )

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  # id
  table <- dplyr::tibble(!!paste0(tableName, "_id") := seq_len(numberRows)) %>%
    correctTable(
      tableName = clinicalTableName, cdmVersion = cdmVersion, warning = FALSE
    )

  # person_id
  if ("person_id" %in% colnames(table)) {
    table <- table %>%
      dplyr::mutate("person_id" = sample(
        x = cdm$person$person_id, size = numberRows, replace = TRUE
      ))
  }

  # dates
  dates <- paste0(clinicalTableName, c("_date", "_start_date", "_end_date"))
  dates <- dates[dates %in% colnames(table)]
  table <- table %>% addDates(dates, cdm$observation_period)

}

addDates <- function(x, cols, observationPeriod) {
  if (length(cols) > 0) {
    for (col in cols) {
      x <- x %>%
        dplyr::mutate(!!col := runif(dplyr::n()))
    }
    sumsum <- paste0(".data[[\"", cols, "\"]]", collapse = " + ")
    x <- x %>%
      dplyr::mutate(cum_sum = !!rlang::parse_expr(sumsum)) %>%
      dplyr::mutate(cum_sum = .data$cum_sum + runif(dplyr::n())) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~ .x/.data$cum_sum)) %>%
      dplyr::select(-"cum_sum")
    observationPeriod <- observationPeriod %>%
      dplyr::mutate(rand = runif(dplyr::n())) %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::filter(.data$rand == min(.data$rand)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"rand")
    x <- x %>%
      dplyr::inner_join(
        observationPeriod %>%
          dplyr::mutate(date_diff = .data$observation_period_end_date -
                          .data$observation_period_start_date
          ) %>%
          dplyr::select(
            "person_id", "start" = "obervation_start_date", "date_diff"
          ),
        by = "person_id"
      ) %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(cols), ~ round(.x * .data$date_diff) + .data$start
      )) %>%
      dplyr::select(-c("start", "date_diff"))
  }
  return(x)
}

#' Function to add a cohort table in a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#' @param tableName Name of the cohort to be created.
#' @param cohortId Ids of the cohort to generate.
#' @param cohortName Names of the cohort to generate.
#' @param seed Seed for random numbers reproducibility.
#' @param cdmVersion Verison of the cdm.
#'
#' @return A cdm_reference object
#'
#' @export
#'
generateCohortTable <- function(cdm,
                                tableName = "cohort",
                                cohortId = 1,
                                cohortName = paste0("cohort_", cohortId),
                                counts = 100,
                                seed = NULL,
                                cdmVersion = attr(cdm, "cdm_version")) {
  # initial checks
  checkInput(
    cdm = cdm, tableName = tableName, cohortId = cohortId,
    cohortName = cohortName, counts = counts, seed = seed,
    cdmVersion = cdmVersion,
    .options = list(cdmRequiredTables = c("person", "observation_period"))
  )

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  # generate cohort table
  cohort <- list()
  for (id in cohortId) {
    if (as.character(id) %in% names(counts)) {
      num <- counts[as.character(id)] %>% unname()
    } else {
      num <- counts[1]
    }
    num <- round(num * nrow(cdm$person))
    cohort[[id]] <- dplyr::tibble(
      cohort_definition_id = id,
      subject_id = sample(
        x = cdm$person$person_id, size = num, replace = TRUE
      )
    ) %>%
      addDates(
        c("cohort_start_date", "cohort_end_date"), cdm$observation_period
      )
  }
  cohort <- dplyr::bind_rows(cohort)

  # generate cohort set table
  cohortSetTable <- dplyr::tibble(
    cohort_definition_id = cohortId, cohort_name = cohortName
  )

  # create class
  cdm[[tableName]] <- newOmopCohort(
    cohortTable = cohort, cohortSetTable = cohortSetTable
  )

  return(cdm)
}

generateCohorts <- function(cdm, cohortNames) {
  for (cohort in cohortNames) {
    cdm <- generateCohortTable(
      cdm = cdm, tableName = cohort, cohortId = 1:3, counts = 0.5
    )
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
