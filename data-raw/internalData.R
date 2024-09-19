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
fieldsCdmTables <- readr::read_csv(
  here::here("data-raw", "OMOP_CDMv5.3_Field_Level.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(cdm_version = "5.3") |>
  dplyr::union_all(
    readr::read_csv(
      here::here("data-raw", "OMOP_CDMv5.4_Field_Level.csv"), show_col_types = FALSE
    ) |>
      dplyr::mutate(cdm_version = "5.4")
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
  cdm_datatype = c("integer", "integer", "date", "date")
) |>
  union_all(tibble(
    cdm_table_name = "cohort_set",
    cdm_field_name = c("cohort_definition_id", "cohort_name"),
    is_required = TRUE,
    cdm_datatype = c("integer", "varchar(255")
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
    )
  )) |>
    union_all(tibble(
      cdm_table_name = "cohort_codelist",
      cdm_field_name = c(
        "cohort_definition_id", "codelist_name", "concept_id" , "type"
      ),
      is_required = TRUE,
      cdm_datatype = c(
        "integer", "varchar(255)", "integer", "varchar(255)"
      )
    ))

fieldsCohorts <- fieldsCohorts |>
  dplyr::mutate(cdm_version = "5.3") |>
  dplyr::union_all(
    fieldsCohorts |>
      dplyr::mutate(cdm_version = "5.4")
  )

fieldsAchilles <- dplyr::tibble(
  cdm_table_name = "achilles_analysis",
  cdm_field_name = c(
    "analysis_id","analysis_name", "stratum_1_name", "stratum_2_name",
    "stratum_3_name", "stratum_4_name", "stratum_5_name", "is_default",
    "category"
  ),
  is_required = TRUE,
  cdm_datatype = c("integer", rep("varchar(255)", 6), "logical", "varchar(255)")
) |>
  dplyr::union_all(dplyr::tibble(
    cdm_table_name = "achilles_results",
    cdm_field_name = c(
      "analysis_id", "stratum_1", "stratum_2", "stratum_3", "stratum_4",
      "stratum_5", "count_value"
    ),
    is_required = TRUE,
    cdm_datatype = c("integer", rep("varchar(255)", 5), "integer")
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
    )
  ))

fieldsAchilles <- fieldsAchilles |>
  dplyr::mutate(cdm_version = "5.3") |>
  dplyr::union_all(
    fieldsAchilles |>
      dplyr::mutate(cdm_version = "5.4")
  )

fieldsTables <- fieldsCdmTables |>
  dplyr::mutate(type = "cdm_table") |>
  dplyr::union_all(
    fieldsCohorts |>
      dplyr::mutate(type = "cohort")
  ) |>
  dplyr::union_all(
    fieldsAchilles |>
      dplyr::mutate(type = "achilles")
  ) |>
  dplyr::group_by(.data$cdm_version) |>
  dplyr::group_split() |>
  as.list()
names(fieldsTables) <- fieldsTables |>
  purrr::map_chr(\(x) unique(x$cdm_version))
fieldsTables <- fieldsTables |>
  purrr::map(\(x) dplyr::select(x, !"cdm_version"))

fieldsResults <- dplyr::tibble(
  result = "summarised_result",
  result_field_name = c(
    "result_id", "cdm_name",
    "group_name", "group_level",
    "strata_name", "strata_level",
    "variable_name", "variable_level",
    "estimate_name", "estimate_type", "estimate_value",
    "additional_name", "additional_level"
  ),
  is_required = TRUE,
  datatype = c("integer", rep("character", 12)),
  na_allowed = c(
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
    FALSE
  ),
  pair = c(
    rep(NA, 2), "name1", "level1", "name2", "level2", rep(NA, 5), "name3",
    "level3"
  )
) |>
  dplyr::union_all(dplyr::tibble(
    result = "compared_result",
    result_field_name = c(
      "result_id", "cdm_name",
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
    datatype = c("integer", rep("character", 18)),
    na_allowed = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
    ),
    pair = c(
      rep(NA, 2), "name1", "level1", "name2", "level2", "name3", "level3",
      "name4", "level4", rep(NA, 5), "name5", "level5", "name6", "level6"
    )
  )) |>
  dplyr::union_all(dplyr::tibble(
    result = "settings",
    result_field_name = c(
      "result_id", "result_type", "package_name", "package_version"
    ),
    is_required = TRUE,
    datatype = c("integer", rep("character", 3)),
    na_allowed = c(
      FALSE, FALSE, FALSE, FALSE
    ),
    pair = c(
      rep(NA, 4)
    )
  ))

groupCount <- c("number subjects", "number records")


fieldTablesColumns <- dplyr::tribble(
  ~"table_name", ~"start_date", ~"end_date", ~"standard_concept", ~"source_concept", ~"type_concept", ~"unique_id", ~"domain_id",
  "observation_period", "observation_period_start_date", "observation_period_end_date", NA, NA, "period_type_concept_id", "observation_period_id","NA",
  "visit_occurrence", "visit_start_date", "visit_end_date", "visit_concept_id", "visit_source_concept_id", "visit_type_concept_id", "visit_occurrence_id","visit",
  "condition_occurrence", "condition_start_date", "condition_end_date", "condition_concept_id", "condition_source_concept_id", "condition_type_concept_id", "condition_occurrence_id","condition",
  "drug_exposure", "drug_exposure_start_date", "drug_exposure_end_date", "drug_concept_id", "drug_source_concept_id", "drug_type_concept_id", "drug_exposure_id","drug",
  "procedure_occurrence", "procedure_date", "procedure_date", "procedure_concept_id", "procedure_source_concept_id", "procedure_type_concept_id", "procedure_occurrence_id","procedure",
  "device_exposure", "device_exposure_start_date", "device_exposure_end_date", "device_concept_id", "device_source_concept_id", "device_type_concept_id", "device_exposure_id","device",
  "measurement", "measurement_date", "measurement_date", "measurement_concept_id", "measurement_source_concept_id", "measurement_type_concept_id", "measurement_id","measurement",
  "observation", "observation_date", "observation_date", "observation_concept_id", "observation_source_concept_id", "observation_type_concept_id", "observation_id","observation",
  "death", "death_date", "death_date", "cause_concept_id", "cause_source_concept_id", "death_type_concept_id", "person_id", "NA"
)

usethis::use_data(
  fieldsTables, fieldsResults, groupCount, fieldTablesColumns, internal = TRUE, overwrite = TRUE
)
