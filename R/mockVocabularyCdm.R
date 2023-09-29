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
#' @examples
#' \donttest{
#' library(CDMUtilities)
#' cdm <- mockVocabularyCdm()
#' cdm
#' }
#'
mockVocabularyCdm <- function(cdmSource = NULL,
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

  # create the list of tables
  cdmTables <- list(
    cdmSource = cdmSource, concept = concept, vocabulary = vocabulary,
    domain = domain, conceptClass = conceptClass,
    conceptRelationship = conceptRelationship, conceptSynonym = conceptSynonym,
    conceptAncestor = conceptAncestor, sourceToConceptMap = sourceToConceptMap,
    drugStrength = drugStrength
  )

  # fill tables
  for (nam in names(cdmTables)) {
    cdmTables <- fillColumns(cdmTables, nam, cdmVersion)
  }
  names(cdmTables) <- toSnakeCase(names(cdmTables))

  cdm <- newCdmReference(
    cdmTables = cdmTables, cdmName = cdmName, cdmVersion = cdmVersion
  )

  return(cdm)
}

fillColumns <- function(cdmTables, tableName, cdm_version) {
  table <- cdmTables[[tableName]]
  if (is.null(table)) {
    table <- defaultTable(tableName)
  } else {
    table <- correctTable(table, tableName, cdm_version)
  }
  cdmTables[[tableName]] <- table
  return(cdmTables)
}
defaultTable <- function(tableName) {
  if (tableName %in% c("conceptRelationship", "conceptSynonym", "sourceToConceptMap")) {
    cols <- fieldsTables %>%
      dplyr::filter(
        .data$cdmTableName == toSnakeCase(tableName),
        .data$isRequired == TRUE, grepl("5.3", .data$cdm_version)
      ) %>%
      dplyr::select("cdmFieldName", "cdmDatatype")
    x <- dplyr::tibble()
    for (k in seq_len(nrow(cols))) {
      x <- x %>%
        dplyr::mutate(!!cols$cdmFieldName[k] := asType(
          NULL, cols$cdmDatatype[k]
        ))
    }
  } else {
    tableName <- paste0(
      "mock",
      substr(toupper(tableName), 1, 1),
      substr(
        tableName, 2, nchar(tableName)
      )
    )
    x <- eval(parse(text = tableName))
  }
  return(x)
}
correctTable <- function(table, tableName, cdmVersion, warning = TRUE) {
  expectedColnames <- fieldsTables %>%
    dplyr::filter(
      grepl(.env$cdmVersion, .data$cdm_version) &
        .data$cdmTableName == .env$tableName
    )
  requiredColnames <- expectedColnames %>% dplyr::pull("cdmFieldName")
  if (!is.null(table)) {
    colnamesToAdd <- setdiff(requiredColnames, colnames(table))
    colnamesToRemove <- setdiff(colnames(table), requiredColnames)
    for (k in seq_along(colnamesToAdd)) {
      type <- expectedColnames %>%
        dplyr::filter(.data$cdmFieldName == .env$colnamesToAdd[k]) %>%
        dplyr::pull("cdmDatatype")
      table <- table %>%
        dplyr::mutate(!!colnamesToAdd[k] := asType(NA, type))
    }
    if (length(colnamesToRemove) > 0 & warning) {
      cli::cli_warn(paste0(
        "Extra columns (", paste0(colnamesToRemove, collapse = ", "),
        ") removed from: ", tableName
      ))
    }
  } else {
    table <- tibble::tibble()
    for (k in seq_len(nrow(expectedColnames))) {
      name <- expectedColnames$cdmFieldName[k]
      type <- expectedColnames$cdmDatatype[k]
      table <- table %>%
        dplyr::mutate(!!name := asType(NULL, type))
    }
  }
  table <- table %>%
    dplyr::select(dplyr::all_of(requiredColnames))
  return(table)
}
