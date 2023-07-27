# Copyright 2022 DARWIN EU (C)
#
# This file is part of CDMUtilities
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

#' It creates a mock database with the vocabulary.
#'
#' @param cdmSource cdm source table.
#' @param concept Concept table.
#' @param vocabulary Vocabulary table
#' @param domain Domain table.
#' @param conceptClass Concept_class table.
#' @param conceptRelationship Concept_relationship table.
#' @param conceptSynonym Concept_synonym table.
#' @param conceptAncestor Concept_ancestor table.
#' @param sourceToConceptMap Source_to_concept_map table.
#' @param drugStrength Drug_strength table.
#' @param cdmVersion cdm version.
#' @param cdmName Name of the cdm.
#'
#' @return A cdm reference with the vocabulary mock tables
#'
#' @export
#'
mockVocabulary <- function(cdmSource = NULL,
                           concept = NULL,
                           vocabulary = NULL,
                           domain = NULL,
                           conceptClass = NULL,
                           conceptRelationship = NULL,
                           conceptSynonym = NULL,
                           conceptAncestor = NULL,
                           sourceToConceptMap = NULL,
                           drugStrength = NULL,
                           cdmVersion = "5.3",
                           cdmName = "MOCK VOCABULARY") {
  # check inputs
  checkInput(
   cdmSource = cdmSource, concept = concept, vocabulary = vocabulary,
   domain = domain, conceptClass = conceptClass,
   conceptRelationship = conceptRelationship, conceptSynonym = conceptSynonym,
   conceptAncestor = conceptAncestor, sourceToConceptMap = sourceToConceptMap,
   drugStrength = drugStrength, cdmVersion = cdmVersion, cdmName = cdmName
  )

  # fill tables
  cdmSource <- fillColumns(cdmSource, "cdm_source", cdmVersion)
  concept <- fillColumns(concept, "concept", cdmVersion)
  vocabulary <- fillColumns(vocabulary, "vocabulary", cdmVersion)
  domain <- fillColumns(domain, "domain", cdmVersion)
  conceptClass <- fillColumns(conceptClass, "concept_class", cdmVersion)
  conceptRelationship <- fillColumns(
    conceptRelationship, "concept_relationship", cdmVersion
  )
  conceptSynonym <- fillColumns(
    conceptSynonym, "concept_synonym", cdmVersion
  )
  conceptAncestor <- fillColumns(
    conceptAncestor, "concept_ancestor", cdmVersion
  )
  sourceToConceptMap <- fillColumns(
    sourceToConceptMap, "source_to_concept_map", cdmVersion
  )
  drugStrength <- fillColumns(drugStrength, "drug_strength", cdmVersion)

  cdm <- newCdmReference(
    cdmTables = list(
      cdm_source = cdmSource, concept = concept, vocabulary = vocabulary,
      domain = domain, concept_class = conceptClass,
      concept_relationship = conceptRelationship,
      concept_synonym = conceptSynonym, concept_ancestor = conceptAncestor,
      source_to_concept_map = sourceToConceptMap, drug_strength = drugStrength
    ),
    cdmName = "MOCK VOCABULARY",
    cdmVersion = cdmVersion,
    validate = FALSE
  )

  return(cdm)
}

fillColumns <- function(table, tableName, cdm_version) {
  if (is.null(table)) {
    return(defaultTable(tableName))
  } else {
    return(correctTable(table, tableName, cdm_version))
  }
}

defaultTable <- function(tableName) {
  tableName <- paste0(
    "mock",
    substr(toupper(tableName), 1, 1),
    substr(
      tableName, 2, nchar(tableName)
    )
  )
  return(eval(parse(text = tableName)))
}

correctTable <- function(table, tableName, cdm_version) {
  expectedColnames <- fieldsTables %>%
    dplyr::filter(
      grepl(.env$cdm_version, .data$cdm_version) &
        .data$cdmTableName == .env$tableName
    )
  requiredColnames <- expectedColnames %>%
    dplyr::filter(.data$isRequired == TRUE) %>%
    dplyr::pull("cdmFieldName")
  optionalColnames <- expectedColnames %>%
    dplyr::filter(.data$isRequired == FALSE) %>%
    dplyr::pull("cdmFieldName")
  colnamesToAdd <- setdiff(requiredColnames, colnames(table))
  colnamesToRemove <- setdiff(
    colnames(table), c(requiredColnames, optionalColnames)
  )
  if (length(colnamesToRemove) > 0) {
    cli::cli_warn(paste0(
      "Extra fields (", paste0(colnamesToRemove, ", "), ") removed from:",
      tableName
    ))
  }
  for (k in seq_along(colnamesToAdd)) {
    type <- expectedColnames %>%
      dplyr::filter(.data$cdmFieldName == .env$colnamesToAdd[k]) %>%
      dplyr::pull("cdmDatatype")
    table <- table %>%
      dplyr::mutate(
        !!expectedColnames[k] := as.character(NA) # correct for type
      )
  }
  table <- table %>%
    dplyr::select(dplyr::any_of(expectedColnames[["cdmFieldName"]]))
  return(table)
}
