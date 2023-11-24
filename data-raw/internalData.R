# Copyright 2022 DARWIN EU (C)
#
# This file is part of OMOPGenerics
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
  select(
    "cdm_table_name" = "cdmTableName",
    "cdm_field_name" = "cdmFieldName",
    "isRequired" = "isRequired",
    "cdm_datatype" = "cdmDatatype",
    "cdm_version"
  )

fieldsCohorts <- tibble(
  cdm_table_name = "cohort",
  cdm_field_name = c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ),
  is_required = TRUE,
  cdm_datatype = c("integer", "integer", "date", "date"),
  cdm_version = "5.3; 5.4"
) |>
  union_all(tibble(
    cdm_table_name = "cohort_set",
    cdm_field_name = c("cohort_definition_id", "cohort_name"),
    is_required = TRUE,
    cdm_datatype = c("integer", "varchar(255"),
    cdm_version = "5.3; 5.4"
  )) |>
  union_all(tibble(
    cdm_table_name = "cohort_attrition",
    cdm_field_name = c(
      "cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"
    ),
    is_required = TRUE,
    cdm_datatype = c(
      "integer", "integer", "integer", "integer", "varchar(255", "integer",
      "integer"
    ),
    cdm_version = "5.3; 5.4"
  ))


usethis::use_data(fieldsTables, fieldsCohorts, internal = TRUE, overwrite = TRUE)
