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

# function to display error message, so it is easier to control in the future
displayErrorMessage <- function(errorMessage) {
  assertCharacter(errorMessage)
  cli::cli_abort(errorMessage)
}

# function to display warning message, so it is easier to control in the future
displayWarningMessage <- function(warningMessage) {
  assertCharacter(warningMessage)
  cli::cli_warn(warningMessage)
}

# Check if it contains a list of omop objects.
checkElements <- function(elements) {
  error <- "Elements must be a named list."
  assertList(elements, named = TRUE, errorMessage = error)
}

# Check if it is a valid path
checkPath <- function(path) {
  if (length(path) != 1 | is.character(path) == FALSE) {
    displayErrorMessage(paste0("path (", path, ") is not a valid path"))
  }
  if (dir.exists(path) == FALSE) {
    displayErrorMessage(paste0("directory (", path, ") does not exist"))
  }
}

# Check if it is a valid resultStem
checkResultsStem <- function(resultsStem) {
  error <- "resultsStem must be a string of length 1 and at least 5 characters"
  assertCharacter(resultsStem, length = 1, minNumCharacter = 5, errorMessage = error)
}

# Check if zip should be displayed
checkZip <- function(zip) {
  assertLogical(zip, length = 1)
}

# Check if it is a list of tables from the same source
checkCdmTables <- function(cdmTables) {
  error <- "cdmTables must be a named list of tables from the same source"
  assertList(x = cdmTables, named = TRUE, errorMessage = error)
}

# Check valid cdm name.
checkCdmName <- function(cdmName) {
  error <- "cdmName must be a character of length one"
  assertCharacter(x = cdmName, length = 1, errorMessage = error)
}

# Check cdm source table.
checkCdmSource <- function(cdmSource) {
  tableCheck(cdmSource, "cdmSource")
}

# Check cdm source table.
checkCdmSource <- function(cdmSource) {
  tableCheck(cdmSource, "cdmSource")
}

# Check cdmSource table.
checkCdmSource <- function(cdmSource){
  tableCheck(cdmSource, "cdmSource")
}

# Check concept table.
checkConcept <- function(concept){
  tableCheck(concept, "concept")
}

# Check vocabulary table.
checkVocabulary <- function(vocabulary){
  tableCheck(vocabulary, "vocabulary")
}

# Check domain table.
checkDomain <- function(domain){
  tableCheck(domain, "domain")
}

# Check conceptClass table.
checkConceptClass <- function(conceptClass){
  tableCheck(conceptClass, "conceptClass")
}

# Check conceptRelationship table.
checkConceptRelationship <- function(conceptRelationship){
  tableCheck(conceptRelationship, "conceptRelationship")
}

# Check conceptSynonym table.
checkConceptSynonym <- function(conceptSynonym){
  tableCheck(conceptSynonym, "conceptSynonym")
}

# Check conceptAncestor table.
checkConceptAncestor <- function(conceptAncestor){
  tableCheck(conceptAncestor, "conceptAncestor")
}

# Check sourceToConceptMap table.
checkSourceToConceptMap <- function(sourceToConceptMap){
  tableCheck(sourceToConceptMap, "sourceToConceptMap")
}

# Check drugStrength table.
checkDrugStrength <- function(drugStrength){
  tableCheck(drugStrength, "drugStrength")
}

tableCheck <- function(table, name) {
  error <- paste0(name, " must be NULL or a table")
  assertTibble(table, null = TRUE, errorMessage = error)
}

# check cdmVersion
checkCdmVersion <- function(cdmVersion) {
  error <- "cdmVersion must be \"5.3\" or \"5.4\"."
  assertChoice(cdmVersion, c("5.3", "5.4"), errorMessage = NULL)
}

# check string
checkString <- function(string) {
  assertCharacter(string, na = TRUE, errorMessage = "string must be a character vector")
}

# check intermediateAsTemp
checkIntermediateAsTemp <- function(intermediateAsTemp) {
  assertLogical(
    intermediateAsTemp, errorMessage = "intermediateAsTemp must be TRUE or FALSE"
  )
}

# check cohortAsTemp
checkCohortAsTemp <- function(cohortAsTemp) {
  assertLogical(
    cohortAsTemp, errorMessage = "cohortAsTemp must be TRUE or FALSE"
  )
}

# check cohortTable
checkCohortTable <- function(cohortTable) {
  assertTibble(cohortTable, columns = c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ))
}

# check cohortSetTable
checkCohortSetTable <- function(cohortSetTable) {
  assertTibble(cohortSetTable, columns = c(
    "cohort_definition_id", "cohort_name"
  ), null = TRUE)
}

# check cohortAttritionTable
checkCohortAttritionTable <- function(cohortAttritionTable) {
  assertTibble(cohortAttritionTable, columns = c(
    "cohort_definition_id", "reason_id", "reason", "number_records",
    "number_subjects", "excluded_records", "excluded_subjects"
  ), null = TRUE)
}

# check cohortCountTable
checkCohortCountTable <- function(cohortCountTable) {
  assertTibble(cohortCountTable, columns = c(
    "cohort_definition_id", "number_records", "number_subjects"
  ), null = TRUE)
}

# check cdmVocabulary
checkCdmVocabulary <- function(cdmVocabulary) {
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
    displayErrorMessage(error)
  }
  if (!all(tables %in% names(cdmVocabulary))) {
    displayErrorMessage(error)
  }
  if (!(attr(cdmVocabulary, "cdm_version") %in% c("5.3", "5.4"))) {
    displayErrorMessage(error)
  }
  return(invisible(cdmVocabulary))
}

# check individuals
checkIndividuals <- function(individuals, person) {
  if (!is.null(individuals)) {
    if (!is.null(person)) {
      displayErrorMessage(
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
      displayErrorMessage(error)
    }
  } else {
    if (is.null(person)) {
      displayErrorMessage("`individuals` or `person` must be supplied.")
    }
  }
  return(invisible(individuals))
}

# check person
checkPerson <- function(person) {
  assertTibble(person, null = TRUE)
}

# check observationPeriod
checkObservationPeriod <- function(observationPeriod) {
  assertTibble(observationPeriod, null = TRUE)
}

# check death
checkDeath <- function(death) {
  assertTibble(death, null = TRUE)
}

# check conditionOccurrence
checkConditionOccurrence <- function(conditionOccurrence) {
  assertTibble(conditionOccurrence, null = TRUE)
}

# check drugExposure
checkDrugExposure <- function(drugExposure) {
  assertTibble(drugExposure, null = TRUE)
}

# check procedureOccurrence
checkProcedureOccurrence <- function(procedureOccurrence) {
  assertTibble(procedureOccurrence, null = TRUE)
}

# check deviceExposure
checkDeviceExposure <- function(deviceExposure) {
  assertTibble(deviceExposure, null = TRUE)
}

# check measurement
checkMeasurement <- function(measurement) {
  assertTibble(measurement, null = TRUE)
}

# check observation
checkObservation <- function(observation) {
  assertTibble(observation, null = TRUE)
}

# check seed
checkSeed <- function(seed) {
  assertNumeric(seed, integerish = TRUE, min = 1, length = 1)
}

# check numberRecords
checkNumberRecords <- function(numberRecords) {
  assertNumeric(numberRecords, min = 0, named = TRUE)
  nam <- c(
    "death", "observationPeriod", "conditionOccurrence", "drugExposure",
    "procedureOccurrence", "deviceExposure", "measurement", "observation"
  )
  if (!all(names(numberRecords) %in% c(nam, "default"))) {
    displayErrorMessage(paste0(
      "possible names for numberRecords: ", paste0(nam, ", ")
    ))
  }
  if (!all(nam %in% names(numberRecords)) && TRUE) {

  }
}
