# Copyright 2022 DARWIN EU (C)
#
# This file is part of omopgenerics
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
    "is_required" = "isRequired",
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
      "integer", "integer", "integer", "integer", "varchar(255)", "integer",
      "integer"
    ),
    cdm_version = "5.3; 5.4"
  )) |>
    union_all(tibble(
      cdm_table_name = "cohort_codelist",
      cdm_field_name = c(
        "cohort_definition_id", "codelist_name", "concept_id" , "type"
      ),
      is_required = TRUE,
      cdm_datatype = c(
        "integer", "varchar(255)", "integer", "varchar(255)"
      ),
      cdm_version = "5.3; 5.4"
    ))

fieldsAchilles <- dplyr::tibble(
  cdm_table_name = "achilles_analysis",
  cdm_field_name = c(
    "analysis_id","analysis_name", "stratum_1_name", "stratum_2_name",
    "stratum_3_name", "stratum_4_name", "stratum_5_name", "is_default",
    "category"
  ),
  is_required = TRUE,
  cdm_datatype = c("integer", rep("varchar(255)", 6), "logical", "varchar(255)"),
  cdm_version = "5.3; 5.4"
) |>
  dplyr::union_all(dplyr::tibble(
    cdm_table_name = "achilles_results",
    cdm_field_name = c(
      "analysis_id", "stratum_1", "stratum_2", "stratum_3", "stratum_4",
      "stratum_5", "count_value"
    ),
    is_required = TRUE,
    cdm_datatype = c("integer", rep("varchar(255)", 5), "integer"),
    cdm_version = "5.3; 5.4"
  )) |>
  dplyr::union_all(dplyr::tibble(
    cdm_table_name = "achilles_results_dist",
    cdm_field_name = c(
      "analysis_id", "stratum_1", "stratum_2", "stratum_3", "stratum_4",
      "stratum_5", "count_value", "min_value", "max_value", "avg_value",
      "stdev_value", "median_value", "p10_value", "p25_value", "p75_value",
      "p90_value"
    ),
    is_required = TRUE,
    cdm_datatype = c(
      "integer", rep("varchar(255)", 5), rep("integer", 3), rep("float", 7)
    ),
    cdm_version = "5.3; 5.4"
  ))

fieldsTables <- fieldsTables |>
  dplyr::mutate(type = "cdm_table") |>
  dplyr::union_all(
    fieldsCohorts |> dplyr::mutate(type = "cohort")
  ) |>
  dplyr::union_all(
    fieldsAchilles |> dplyr::mutate(type = "achilles")
  )

fieldsResults <- dplyr::tibble(
  result = "summarised_result",
  result_field_name = c(
    "result_id",
    "group_name", "group_level",
    "strata_name", "strata_level",
    "variable_name", "variable_level",
    "estimate_name", "estimate_type", "estimate_value",
    "additional_name", "additional_level"
  ),
  is_required = TRUE,
  datatype = c("integer", rep("character", 11)),
  na_allowed = c(
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
    FALSE
  ),
  pair = c(
    rep(NA, 1), "name1", "level1", "name2", "level2", rep(NA, 5), "name3",
    "level3"
  )
) |>
  dplyr::union_all(dplyr::tibble(
    result = "compared_result",
    result_field_name = c(
      "result_id",
      "group_name_reference", "group_level_reference",
      "strata_name_reference", "strata_level_reference",
      "group_name_comparator", "group_level_comparator",
      "strata_name_comparator", "strata_level_comparator",
      "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value",
      "additional_name_reference", "additional_level_reference",
      "additional_name_comparator", "additional_level_comparator"
    ),
    is_required = TRUE,
    datatype = c("integer", rep("character", 17)),
    na_allowed = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
    ),
    pair = c(
      rep(NA, 1), "name1", "level1", "name2", "level2", "name3", "level3",
      "name4", "level4", rep(NA, 5), "name5", "level5", "name6", "level6"
    )
  ))

usethis::use_data(
  fieldsTables, fieldsResults, internal = TRUE, overwrite = TRUE
)
