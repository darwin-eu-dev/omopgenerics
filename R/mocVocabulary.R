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
                           drug_strength = NULL) {
  # check inputs
  checkInput(
    cdm_source = cdm_source, concept = concept, vocabulary = vocabulary,
    domain = domain, concept_class = concept_class,
    concept_class= concept_class, concept_relationship = concept_relationship,
    concept_synonym = concept_synonym, concept_ancestor = concept_ancestor,
    source_to_concept_map = source_to_concept_map,
    drug_strength = drug_strength
  )

  # fill tables
  cdm_source <- fillCdmSource(cdm_source)
  concept <- fillConcept(concept)

  newCdmReference(
    cdmTables = list(
      cdm_source, concept, vocabulary, domain, concept_class,
      concept_relationship, concept_synonym, concept_ancestor,
      source_to_concept_map, drug_strength
    ),
    cdmName = "MOCK VOCABULARY"
  )
}
