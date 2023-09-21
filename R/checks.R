# Copyright 2023 DARWIN EU (C)
#
# This file is part of PatientProfiles
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

# Check if it contains a list of omop objects.
checkElements <- function(elements, call = parent.frame()) {
  error <- "Elements must be a named list."
  assertList(elements, named = TRUE, errorMessage = error)
}

# Check if it is a valid path
checkPath <- function(path, call = parent.frame()) {
  if (length(path) != 1 | is.character(path) == FALSE) {
    cli::cli_abort(paste0("path (", path, ") is not a valid path"), call = call)
  }
  if (dir.exists(path) == FALSE) {
    cli::cli_abort(paste0("directory (", path, ") does not exist"), call = call)
  }
}

# Check if it is a valid resultStem
checkResultsStem <- function(resultsStem, call = parent.frame()) {
  error <- "resultsStem must be a string of length 1 and at least 5 characters"
  assertCharacter(resultsStem, length = 1, minNumCharacter = 5, errorMessage = error)
}

# Check if zip should be displayed
checkZip <- function(zip, call = parent.frame()) {
  assertLogical(zip, length = 1)
}

# Check if it is a list of tables from the same source
checkCdmTables <- function(cdmTables, call = parent.frame()) {
  error <- "cdmTables must be a named list of tables from the same source"
  assertList(x = cdmTables, named = TRUE, errorMessage = error)
}

# Check valid cdm name.
checkCdmName <- function(cdmName, call = parent.frame()) {
  error <- "cdmName must be a character of length one"
  assertCharacter(x = cdmName, length = 1, errorMessage = error)
}

# Check cdm source table.
checkCdmSource <- function(cdmSource, call = parent.frame()) {
  tableCheck(cdmSource, "cdmSource")
}

# Check cdm source table.
checkCdmSource <- function(cdmSource, call = parent.frame()) {
  tableCheck(cdmSource, "cdmSource")
}

# Check cdmSource table.
checkCdmSource <- function(cdmSource, call = parent.frame()){
  tableCheck(cdmSource, "cdmSource")
}

# Check concept table.
checkConcept <- function(concept, call = parent.frame()){
  tableCheck(concept, "concept")
}

# Check vocabulary table.
checkVocabulary <- function(vocabulary, call = parent.frame()){
  tableCheck(vocabulary, "vocabulary")
}

# Check domain table.
checkDomain <- function(domain, call = parent.frame()){
  tableCheck(domain, "domain")
}

# Check conceptClass table.
checkConceptClass <- function(conceptClass, call = parent.frame()){
  tableCheck(conceptClass, "conceptClass")
}

# Check conceptRelationship table.
checkConceptRelationship <- function(conceptRelationship, call = parent.frame()){
  tableCheck(conceptRelationship, "conceptRelationship")
}

# Check conceptSynonym table.
checkConceptSynonym <- function(conceptSynonym, call = parent.frame()){
  tableCheck(conceptSynonym, "conceptSynonym")
}

# Check conceptAncestor table.
checkConceptAncestor <- function(conceptAncestor, call = parent.frame()){
  tableCheck(conceptAncestor, "conceptAncestor")
}

# Check sourceToConceptMap table.
checkSourceToConceptMap <- function(sourceToConceptMap, call = parent.frame()){
  tableCheck(sourceToConceptMap, "sourceToConceptMap")
}

# Check drugStrength table.
checkDrugStrength <- function(drugStrength, call = parent.frame()){
  tableCheck(drugStrength, "drugStrength")
}

tableCheck <- function(table, name, call = parent.frame()) {
  error <- paste0(name, " must be NULL or a table")
  assertTibble(table, null = TRUE, errorMessage = error)
}

# check cdmVersion
checkCdmVersion <- function(cdmVersion, call = parent.frame()) {
  error <- "cdmVersion must be \"5.3\" or \"5.4\"."
  assertChoice(cdmVersion, c("5.3", "5.4"), errorMessage = NULL)
}

# check string
checkString <- function(string, call = parent.frame()) {
  assertCharacter(string, na = TRUE, errorMessage = "string must be a character vector")
}

# check intermediateAsTemp
checkIntermediateAsTemp <- function(intermediateAsTemp, call = parent.frame()) {
  assertLogical(
    intermediateAsTemp, errorMessage = "intermediateAsTemp must be TRUE or FALSE"
  )
}

# check cohortAsTemp
checkCohortAsTemp <- function(cohortAsTemp, call = parent.frame()) {
  assertLogical(
    cohortAsTemp, errorMessage = "cohortAsTemp must be TRUE or FALSE"
  )
}

# check cohortTable
checkCohortTable <- function(cohortTable, call = parent.frame()) {
  assertTibble(cohortTable, columns = c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ))
}

# check cohortSetTable
checkCohortSetTable <- function(cohortSetTable, call = parent.frame()) {
  assertTibble(cohortSetTable, columns = c(
    "cohort_definition_id", "cohort_name"
  ), null = TRUE)
}

# check cohortAttritionTable
checkCohortAttritionTable <- function(cohortAttritionTable, call = parent.frame()) {
  assertTibble(cohortAttritionTable, columns = c(
    "cohort_definition_id", "reason_id", "reason", "number_records",
    "number_subjects", "excluded_records", "excluded_subjects"
  ), null = TRUE)
}

# check cohortCountTable
checkCohortCountTable <- function(cohortCountTable, call = parent.frame()) {
  assertTibble(cohortCountTable, columns = c(
    "cohort_definition_id", "number_records", "number_subjects"
  ), null = TRUE)
}

# check cdmVocabulary
checkCdmVocabulary <- function(cdmVocabulary, call = parent.frame()) {
  tables <- c(
    "cdm_source", "concept", "vocabulary", "domain", "concept_class",
    "concept_relationship", "concept_synonim", "concept_ancestor",
    "source_to_concept_map", "drug_strength"
  )
  error <- paste0(
    "cdmVocabulary must be a `cdm_reference` with the following tables: ",
    paste0(tables, collapse = ", "), "; with a valid cdm_version."
  )
  if (!("cdm_reference" %in% class(cdmVocabulary))) {
    cli::cli_abort(error)
  }
  if (!all(tables %in% names(cdmVocabulary))) {
    cli::cli_abort(error)
  }
  if (!(attr(cdmVocabulary, "cdm_version") %in% c("5.3", "5.4"))) {
    cli::cli_abort(error)
  }
  return(invisible(cdmVocabulary))
}

# check individuals
checkIndividuals <- function(individuals, person, call = parent.frame()) {
  if (!is.null(individuals)) {
    if (!is.null(person)) {
      cli::cli_abort(
        "individuals and person are not compatible arguments one must be NULL"
      )
    }
    columns <- c(
      "number_individuals", "sex", "year_birth", "observation_start",
      "observation_end"
    )
    error <- paste0(
      "individuals must be a number or a tibble with the following columns: ",
      paste0(columns, collapse = ", "), "."
    )
    if (is.numeric(individuals)) {
      assertNumeric(
        x = individuals, integerish = TRUE, length = 1, errorMessage = error
      )
    } else if ("tbl" %in% class(individuals)) {
      assertTibble(individuals, columns = columns, errorMessage = error)
    } else if (is.null(individuals) & is.null(person)){

    } else {
      cli::cli_abort(error)
    }
  } else {
    if (is.null(person)) {
      cli::cli_abort("`individuals` or `person` must be supplied.")
    }
  }
  return(invisible(individuals))
}

# check person
checkPerson <- function(person, call = parent.frame()) {
  assertTibble(person, null = TRUE)
}

# check observationPeriod
checkObservationPeriod <- function(observationPeriod, call = parent.frame()) {
  assertTibble(observationPeriod, null = TRUE)
}

# check death
checkDeath <- function(death, call = parent.frame()) {
  assertTibble(death, null = TRUE)
}

# check conditionOccurrence
checkConditionOccurrence <- function(conditionOccurrence, call = parent.frame()) {
  assertTibble(conditionOccurrence, null = TRUE)
}

# check drugExposure
checkDrugExposure <- function(drugExposure, call = parent.frame()) {
  assertTibble(drugExposure, null = TRUE)
}

# check procedureOccurrence
checkProcedureOccurrence <- function(procedureOccurrence, call = parent.frame()) {
  assertTibble(procedureOccurrence, null = TRUE)
}

# check deviceExposure
checkDeviceExposure <- function(deviceExposure, call = parent.frame()) {
  assertTibble(deviceExposure, null = TRUE)
}

# check measurement
checkMeasurement <- function(measurement, call = parent.frame()) {
  assertTibble(measurement, null = TRUE)
}

# check observation
checkObservation <- function(observation, call = parent.frame()) {
  assertTibble(observation, null = TRUE)
}

# check seed
checkSeed <- function(seed, call = parent.frame()) {
  assertNumeric(seed, integerish = TRUE, min = 1, length = 1)
}

# check numberRecords
checkNumberRecords <- function(numberRecords, call = parent.frame()) {
  assertNumeric(numberRecords, min = 0, named = TRUE)
  nam <- c(
    "death", "observationPeriod", "conditionOccurrence", "drugExposure",
    "procedureOccurrence", "deviceExposure", "measurement", "observation"
  )
  if (!all(names(numberRecords) %in% c(nam, "default"))) {
    cli::cli_abort(paste0(
      "possible names for numberRecords: ", paste0(nam, ", ")
    ))
  }
  if (!all(nam %in% names(numberRecords)) && TRUE) {

  }
}
