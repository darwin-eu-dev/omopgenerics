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

library(dplyr)

# add the mock vocabulary data
mockDrugStrength <- readr::read_csv(
  here::here("data-raw", "drugStrength.csv"), show_col_types = FALSE
)
mockConcept <- readr::read_csv(
  here::here("data-raw", "concept.csv"), show_col_types = FALSE
)
mockConceptAncestor <- readr::read_csv(
  here::here("data-raw", "conceptAncestor.csv"), show_col_types = FALSE
)
mockCdmSource <- readr::read_csv(
  here::here("data-raw", "cdmSource.csv"), show_col_types = FALSE
)
mockVocabulary <- readr::read_csv(
  here::here("data-raw", "vocabulary.csv"), show_col_types = FALSE
)
mockDomain <- readr::read_csv(
  here::here("data-raw", "domain.csv"), show_col_types = FALSE
)
mockConceptClass <- readr::read_csv(
  here::here("data-raw", "conceptClass.csv"), show_col_types = FALSE
)

# add the information related to the different domains
domainInformation <- readr::read_csv(
  here::here("data-raw", "domain_information.csv"), show_col_types = FALSE
)

# fields and type of tables
fieldsTables53 <- readr::read_csv(
  here::here("data-raw", "OMOP_CDMv5.3_Field_Level.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(cdm_version_53 = 1)
fieldsTables54 <- readr::read_csv(
  here::here("data-raw", "OMOP_CDMv5.4_Field_Level.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(cdm_version_54 = 1)
fieldsTables <- fieldsTables53 |>
  full_join(
    fieldsTables54,
    by = c("cdmTableName", "cdmFieldName", "isRequired", "cdmDatatype")
  ) |>
  mutate(
    cdm_version = case_when(
      cdm_version_53 == 1 & cdm_version_54 == 1 ~ "5.3; 5.4",
      cdm_version_53 == 1 & is.na(cdm_version_54) ~ "5.3",
      is.na(cdm_version_53) & cdm_version_54 == 1 ~ "5.4"
    )
  ) |>
  select(-"cdm_version_53", -"cdm_version_54")

# data to generate clinical tables
mockClinicalFields <- dplyr::tribble(
  ~cdmTableName,        ~cdmFieldName,           ~cdmDataType, ~generate,
  "condition_occurrence", "condition_occurrence_id", "integer", "number_row",
  "condition_occurrence", "person_id", "integer", "person_id",
  "condition_occurrence", "condition_concept_id", "integer", "concept_id",
  "condition_occurrence", "condition_start_date", "date", "date_1",
  "condition_occurrence", "condition_start_datetime", "datetime", "datetime_1",
  "condition_occurrence", "condition_end_date", "date", "date_2",
  "condition_occurrence", "condition_end_datetime", "datetime", "datetime_2",
  "condition_occurrence", "condition_type_concept_id", "integer", "type_concept_id",
  "drug_exposure", "drug_exposure_id", "integer", "number_row",
  "drug_exposure", "person_id", "integer", "person_id",
  "drug_exposure", "drug_concept_id", "integer", "concept_id",
  "drug_exposure", "drug_exposure_start_date", "date", "date_1",
  "drug_exposure", "drug_exposure_start_datetime", "datetime", "datetime_1",
  "drug_exposure", "drug_exposure_end_date", "date", "date_2",
  "drug_exposure", "drug_exposure_end_datetime", "datetime", "datetime_2",
  "drug_exposure", "drug_type_concept_id", "integer", "type_concept_id",
  "drug_exposure", "quantity", "float", "rand_1_100",
  "drug_exposure", "days_supply", "integer", "duration",
  "observation", "observation_id", "integer", "number_row",
  "observation", "person_id", "integer", "person_id",
  "observation", "observation_concept_id", "integer", "concept_id",
  "observation", "observation_date", "date", "date1",
  "observation", "observation_datetime", "datetime", "datetime1",
  "observation", "observation_type_concept_id", "integer", "type_concept_id",
  "procedure_occurrence", "procedure_occurrence_id", "integer", "number_row",
  "procedure_occurrence", "person_id", "integer", "person_id",
  "procedure_occurrence", "procedure_concept_id", "integer", "concept_id",
  "procedure_occurrence", "procedure_date", "date", "date_1",
  "procedure_occurrence", "procedure_datetime", "datetime", "datetime_1",
  "procedure_occurrence", "procedure_type_concept_id", "integer", "type_concept_id",
  "procedure_occurrence", "procedure_end_date", "date", "date_2",
  "procedure_occurrence", "procedure_end_datetime", "datetime", "datetime_2",
  "device_exposure", "device_exposure_id", "integer", "number_row",
  "device_exposure", "person_id", "integer", "person_id",
  "device_exposure", "device_concept_id", "integer", "concept_id"
  "device_exposure", "device_exposure_start_date", "date",
  "device_exposure", "device_exposure_start_datetime", "datetime",
  "device_exposure", "device_exposure_end_date", "date",
  "device_exposure", "device_exposure_end_datetime", "datetime",
  "device_exposure", "device_type_concept_id", "integer",
  "device_exposure", "unique_device_id", "varchar(50)",
  "device_exposure", "quantity", "integer",
  "device_exposure", "provider_id", "integer",
  "device_exposure", "visit_occurrence_id", "integer",
  "device_exposure", "visit_detail_id", "integer",
  "device_exposure", "device_source_value", "varchar(50)",
  "device_exposure", "device_source_concept_id", "integer",
  "device_exposure", "unique_device_id", "varchar(255)",
  "device_exposure", "production_id", "varchar(255)",
  "device_exposure", "unit_concept_id", "integer",
  "device_exposure", "unit_source_value", "varchar(50)",
  "device_exposure", "unit_source_concept_id", "integer",
  "measurement",
  "death",
)

usethis::use_data(
  mockDrugStrength, mockConcept, mockConceptAncestor, domainInformation,
  mockCdmSource, mockVocabulary, mockDomain, mockConceptClass, fieldsTables,
  internal = TRUE, overwrite = TRUE
)
