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
#' @param concept Concept table.
#' @param vocabulary Vocabulary table
#' @param domain Doamin table.
#' @param concept_class Concept_class table.
#' @param concept_relationship Concept_relationship table.
#' @param concept_synonym Concept_synonym table.
#' @param concept_ancestor Concept_ancestor table.
#' @param soource_to_concept_map Source_to_concept_map table.
#' @param drug_strength Drug_strength table.
#'
#' @return A cdm reference with the vocabulary mock tables
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMUtilities)
#'
#' cdm <- mockVocabulary()
#'
#' cdm
#' }
#'
mockVocabulary <- function(concept = NULL,
                           vocabulary = NULL,
                           domain = NULL,
                           concept_class = NULL,
                           concept_relationship = NULL,
                           concept_synonym = NULL,
                           concept_ancestor = NULL,
                           source_to_concept_map = NULL,
                           drug_strength = NULL,
                           cdm_version = "5.3") {
  # check inputs
  checkInput(
    cdm_source = cdm_source, concept = concept, vocabulary = vocabulary,
    domain = domain, concept_class = concept_class,
    concept_class= concept_class, concept_relationship = concept_relationship,
    concept_synonym = concept_synonym, concept_ancestor = concept_ancestor,
    source_to_concept_map = source_to_concept_map,
    drug_strength = drug_strength, cdm_version = cdm_version
  )

  # fill tables
  cdm_source <- fillColumns(cdm_source, "cdm_source", cdm_version)
  concept <- fillColumns(concept, "concept", cdm_version)
  vocabulary <- fillColumns(vocabulary, "vocabulary", cdm_version)
  domain <- fillColumns(domain, "domain", cdm_version)
  concept_class <- fillColumns(concept_class, "concept_class", cdm_version)
  concept_relationship <- fillColumns(
    concept_relationship, "concept_relationship", cdm_version
  )
  concept_synonym <- fillColumns(
    concept_synonym, "concept_synonym", cdm_version
  )
  concept_ancestor <- fillColumns(
    concept_ancestor, concept_ancestor, cdm_version
  )
  source_to_concept_map <- fillColumns(
    source_to_concept_map, "source_to_concept_map", cdm_version
  )
  drug_strength <- fillColumns(drug_strength, "drug_strength", cdm_version)

  newCdmReference(
    cdmTables = list(
      cdm_source, concept, vocabulary, domain, concept_class,
      concept_relationship, concept_synonym, concept_ancestor,
      source_to_concept_map, drug_strength
    ),
    cdmName = "MOCK VOCABULARY"
  )
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
