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
  here::here("data-raw", "drug_strength.csv"), show_col_types = FALSE
)
mockConcept <- readr::read_csv(
  here::here("data-raw", "concept.csv"), show_col_types = FALSE
)
mockConceptAncestor <- readr::read_csv(
  here::here("data-raw", "concept_ancestor.csv"), show_col_types = FALSE
)

# add the information related to the different domains
domainInformation <- readr::read_csv(
  here::here("data-raw", "domain_information.csv"), show_col_types = FALSE
)

# fields and type of tables
fieldsTables53 <- readr::read_csv(
  here::here("data-raw", "OMOP_CDMv5.3_Field_Level.csv"), show_col_types = FALSE
) %>%
  dplyr::mutate(cdm_version_53 = 1)
fieldsTables54 <- readr::read_csv(
  here::here("data-raw", "OMOP_CDMv5.4_Field_Level.csv"), show_col_types = FALSE
) %>%
  dplyr::mutate(cdm_version_54 = 1)
fieldsTables <- fieldsTables53 %>%
  full_join(
    fieldsTables54,
    by = c("cdmTableName", "cdmFieldName", "isRequired", "cdmDatatype")
  ) %>%
  mutate(
    cdm_version = case_when(
      cdm_version_53 == 1 & cdm_version_54 == 1 ~ "5.3; 5.4",
      cdm_version_53 == 1 & is.na(cdm_version_54) ~ "5.3",
      is.na(cdm_version_53) & cdm_version_54 == 1 ~ "5.4"
    )
  ) %>%
  select(-"cdm_version_53", -"cdm_version_54")

usethis::use_data(
  mockDrugStrength, mockConcept, mockConceptAncestor, domainInformation,
  fieldsTables, internal = TRUE, overwrite = TRUE
)
