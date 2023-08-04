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
checkElements <- function(elements) {
  error <- "Elements must be a named list"
  assertList(elements, named = TRUE, error = error, uniqueType = FALSE)
}

# Check if it is a valid path
checkPath <- function(path) {
  if (length(path) != 1 | is.character(path) == FALSE) {
    cli::cli_abort("path is not a valid path")
  }
  if (dir.exists(path) == FALSE) {
    cli::cli_abort(paste0("directory (", path, ") does not exist"))
  }
}

# Check if it is a valid resultStem
checkResultsStem <- function(resultsStem) {
  error <- "resultsStem must be a string of length 1 and at least 5 characters"
  if (!is.character(resultsStem) | length(resultsStem) != 1) {
    cli::cli_abort(error)
  }
  if (nchar(resultsStem) < 5) {
    cli::cli_abort(error)
  }
}

# Check if zip should be displayed
checkZip <- function(zip) {
  if (length(zip != 1) | !is.logical(zip) | is.na(zip)) {
    cli::cli_abort("zip should be TRUE or FALSE")
  }
}

# Check if it is a list of tables from the same source
checkListTables <- function(listTables) {
  error <- "listTables must be a named list of tables from the same source"
  assertList(
    x = listTables, named = TRUE, types = c("tbl_sql", "tbl"),
    uniqueType = TRUE, error = error
  )
}

# Check valid cdm name.
checkCdmName <- function(cdmName) {
  error <- "cdmName must be a character of length one"
  assertCharacter(x = cdmName, len = 1, error = error)
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
  if (!is.null(table) | !("tbl" %in% class(table))) {
    cli::cli(paste0(name, " must be NULL or a table"))
  }
}

# check cdmVersion
checkCdmVersion <- function(cdmVersion) {
  error <- "cdmVersion must be \"5.3\" or \"5.4\"."
  assertChoice(cdmVersion, c("5.3", "5.4"), error)
}

# check string
checkString <- function(string) {
  if (!is.character(string)) {
    cli::cli_abort("string must be a character vector")
  }
}

# check intermediateAsTemp
checkIntermediateAsTemp <- function(intermediateAsTemp) {
  assertLogical(
    intermediateAsTemp, error = "intermediateAsTemp must be TRUE or FALSE"
  )
}

# check cohortAsTemp
checkCohortAsTemp <- function(cohortAsTemp) {
  assertLogical(
    cohortAsTemp, error = "cohortAsTemp must be TRUE or FALSE"
  )
}

# checkInput(
#   cdmVocabuly = cdmVocabuly, cdmName = cdmName, individuals = individuals,
#   person = person, observation_period = observation_period, death = death,
#   condition_occurrence = condition_occurrence, drug_exposure = drug_exposure,
#   procedure_occurrence = procedure_occurrence,
#   device_exposure = device_exposure, measurement = measurement,
#   observation = observation, seed = seed, cdmTables = list(...)
# )
# checkInput(cdmTables = cdmTables, cdmName = cdmName)
#
# checkInput(
#   cdm_source = cdm_source, concept = concept, vocabulary = vocabulary,
#   domain = domain, concept_class = concept_class,
#   concept_class= concept_class, concept_relationship = concept_relationship,
#   concept_synonym = concept_synonym, concept_ancestor = concept_ancestor,
#   source_to_concept_map = source_to_concept_map,
#   drug_strength = drug_strength, cdm_version = cdm_version
# )
#

