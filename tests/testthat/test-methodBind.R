test_that("bind no class", {
  x <- 1
  expect_error(bind(x))
})

test_that("bind a cohort_table", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = 1:3,
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2010-01-05")
  )
  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(2, 2, 3, 3, 3),
    subject_id = c(1, 2, 3, 1, 2),
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2010-01-05")
  )
  cohort3 <- dplyr::tibble(
    cohort_definition_id = 1:5,
    subject_id = c(1, 2, 3, 1, 2),
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2010-01-05")
  )
  cohort4 <- cohort3
  attr(cohort4, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1:5,
    cohort_name = c("first_cohort", "second_cohort", "third_cohort", "fourth_cohort", "fifth_cohort")
  )
  cdm <- cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        person_id = c(1L, 2L, 3L), gender_concept_id = 0L, year_of_birth = 1990L,
        race_concept_id = 0L, ethnicity_concept_id = 0L
      ),
      "observation_period" = dplyr::tibble(
        observation_period_id = c(1L, 2L, 3L), person_id = c(1L, 2L, 3L),
        observation_period_start_date = as.Date("2000-01-01"),
        observation_period_end_date = as.Date("2023-12-31"),
        period_type_concept_id = 0L
      )
    ),
    cdmName = "mock",
    cohortTables = list(
      "cohort1" = cohort1, "cohort2" = cohort2, "cohort3" = cohort3,
      "cohort4" = cohort4
    )
  )
  expect_no_error(newcdm <- bind(cdm$cohort1, cdm$cohort2, name = "new_cohort"))
  expect_true("new_cohort" %in% names(newcdm))
  expect_true(inherits(newcdm$new_cohort, "cohort_table"))
  expect_true(all(c("cohort_set", "cohort_attrition") %in% names(attributes(newcdm$new_cohort))))
  expect_identical(
    settings(newcdm$new_cohort) |> data.frame(),
    settings(newcdm$cohort1) |> dplyr::bind_rows(settings(newcdm$cohort2)) |> data.frame()
  )
  expect_identical(
    attrition(newcdm$new_cohort) |> data.frame(),
    attrition(newcdm$cohort1) |> dplyr::bind_rows(attrition(newcdm$cohort2)) |> data.frame()
  )
  expect_error(bind(cdm$cohort1, cdm$cohort2))
  expect_error(bind(cdm$cohort1, cdm$cohort2, name = NA_character_))
  expect_error(bind(cdm$cohort1, cdm$cohort2, name = 1))
  expect_error(bind(cdm$cohort1, cdm$cohort2, name = c("a", "b")))
  expect_no_error(bind(cdm$cohort1, name = "new_cohort"))
  expect_error(bind(cdm$cohort1, cdm$cohort3, name = "new_cohort"))
  expect_error(bind(cdm$cohort2, cdm$cohort3, name = "new_cohort"))
  expect_error(bind(cdm$cohort1, cdm$cohort2, cdm$cohort3, name = "new_cohort"))
  expect_no_error(newcdm <- bind(cdm$cohort1, cdm$cohort4, name = "new_cohort"))
  expect_no_error(newcdm <- bind(cdm$cohort2, cdm$cohort4, name = "new_cohort"))
  expect_no_error(newcdm <- bind(cdm$cohort3, cdm$cohort4, name = "new_cohort"))
  expect_no_error(newcdm <- bind(cdm$cohort1, cdm$cohort2, cdm$cohort4, name = "new_cohort"))
  expect_equal(newcdm$new_cohort |> dplyr::tally() |> dplyr::pull(), 13)
  expect_equal(settings(newcdm$new_cohort)$cohort_definition_id, 1:8)
  expect_equal(
    settings(newcdm$new_cohort)$cohort_name,
    c(
      settings(newcdm$cohort1)$cohort_name,
      settings(newcdm$cohort2)$cohort_name,
      settings(newcdm$cohort4)$cohort_name
    )
  )
  expect_equal(attrition(newcdm$new_cohort) |> nrow(), 8)

  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = 1:3,
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2010-01-05"),
    extra_column1 = 1
  )
  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(2, 2, 3, 3, 3),
    subject_id = c(1, 2, 3, 1, 2),
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2010-01-05"),
    extra_column2 = TRUE,
    extra_column3 = "fjhhl"
  )
  cohort3 <- dplyr::tibble(
    cohort_definition_id = 1:5,
    subject_id = c(1, 2, 3, 1, 2),
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2010-01-05")
  )
  attr(cohort3, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1:5,
    cohort_name = c(
      "first_cohort", "second_cohort", "third_cohort", "fourth_cohort",
      "fifth_cohort"
    )
  )
  cdm <- cdmFromTables(
    tables = list(
      "person" = dplyr::tibble(
        person_id = c(1L, 2L, 3L), gender_concept_id = 0L, year_of_birth = 1990L,
        race_concept_id = 0L, ethnicity_concept_id = 0L
      ),
      "observation_period" = dplyr::tibble(
        observation_period_id = c(1L, 2L, 3L), person_id = c(1L, 2L, 3L),
        observation_period_start_date = as.Date("2000-01-01"),
        observation_period_end_date = as.Date("2023-12-31"),
        period_type_concept_id = 0L
      )
    ),
    cdmName = "mock",
    cohortTables = list(
      "cohort1" = cohort1, "cohort2" = cohort2, "cohort3" = cohort3
    )
  )

  expect_no_error(
    cdm <- bind(cdm$cohort1, cdm$cohort2, cdm$cohort3, name = "cohort6")
  )
  expect_true(all(
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "extra_column1", "extra_column2", "extra_column3") %in%
      colnames(cdm$cohort6)
  ))
})

test_that("bind summarised_result", {
  x <- dplyr::tibble(
    "group_name" = "overall",
    "group_level" = "overall",
    "strata_name" = c("age_group", "age_group", "sex", "sex"),
    "strata_level" = c("<40", ">40", "Female", "Male"),
    "variable_name" = "number subjects",
    "variable_level" = NA_character_,
    "estimate_name" = "count",
    "estimate_type" = "integer",
    "estimate_value" = sample.int(100, 4) |> as.character(),
    "additional_name" = "overall",
    "additional_level" = "overall"
  )
  # empty summarisedResult
  res1 <- emptySummarisedResult()
  # no settings
  res2 <- x |>
    dplyr::mutate("result_id" = 1L, "cdm_name" = "cprd_gold") |>
    newSummarisedResult()
  res3 <- x |>
    dplyr::mutate("result_id" = 1L, "cdm_name" = "cprd_gold") |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 1L, result_type = "custom"
    ))
  res4 <- x |>
    dplyr::mutate("result_id" = 3L, "cdm_name" = "cprd_gold") |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 3L, result_type = "custom", param = TRUE
    ))
  res5 <- x |>
    dplyr::mutate("result_id" = 1L, "cdm_name" = "eunomia") |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 1L, result_type = "custom"
    ))
  res6 <- x |>
    dplyr::mutate("result_id" = 5L, "cdm_name" = "cprd_gold") |>
    newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 5L, washout = 35
    ))

  # two summarised_result
  expect_no_error(new1 <- bind(res1, res2, res3, res4, res5, res6))
  expect_identical(settings(new1), dplyr::tibble(
    "result_id" = c(1L, 2L, 3L, 4L),
    "result_type" = c(NA, "custom", "custom", NA),
    "param" = c(NA, NA, TRUE, NA),
    "washout" = c(NA, NA, NA, 35)
  ))
  attr(new1, "settings") <- NULL
  expect_identical(
    new1 |> dplyr::count(.data$result_id) |> dplyr::as_tibble(),
    dplyr::tibble("result_id" = c(1L, 2L, 3L, 4L), "n" = c(4L, 8L, 4L, 4L))
  )

  # one of the summarised_result does not have settings
  expect_no_error(new2 <- bind(res2, res3))

  # if we bind the same summarised_result we get an error because number of
  # subjects is repeated
  expect_equal(bind(res2, res2), res2)

  # repeated settings
  expect_no_error(new4 <- bind(res3, res5))
  expect_identical(new4$result_id |> unique(), 1L)
  expect_identical(
    new4 |> settings() |> dplyr::pull("result_id") |> unique(), 1L
  )

  # repeated settings and empty stetings
  expect_no_error(new5 <- bind(res3, res5, res2))
  expect_identical(new5$result_id |> unique(), c(1L, 2L))
  expect_identical(
    new5 |> settings() |> dplyr::pull("result_id") |> unique(), c(1L, 2L)
  )

  # empty results with no settings
  expect_identical(bind(emptySummarisedResult()),
                   emptySummarisedResult())
  expect_identical(bind(emptySummarisedResult(),
                        emptySummarisedResult()),
                   emptySummarisedResult())

  # empty results with settings
  expect_identical(bind(emptySummarisedResult(settings = dplyr::tibble(result_id = 1L,
                                                                       a = "a"))),
                   emptySummarisedResult(settings = dplyr::tibble(result_id = 1L,
                                                                  a = "a")))
  expect_identical(bind(emptySummarisedResult(settings = dplyr::tibble(result_id = 1L,
                                                                       a = "a")),
                        emptySummarisedResult(settings = dplyr::tibble(result_id = 2L,
                                                                       a = "b"))),
                   emptySummarisedResult(settings = dplyr::tibble(result_id = c(1L, 2L),
                                                                  a = c("a", "b"))))

  # empty elements
  expect_no_error(bind(NULL))
  expect_no_error(bind(res3, emptySummarisedResult()))
  expect_no_error(bind(res3, NULL))
  expect_no_error(bind(NULL, res3))
  expect_no_error(bind(list(NULL, res3)))
  expect_no_error(bind(list(res3, NULL)))
  expect_no_error(bind(NULL, list(NULL, res3)))
  expect_no_error(bind(NULL, list(res3, NULL)))
  # do we want this to work?
  expect_error(bind(list(NULL, res3), NULL))
  expect_error(bind(list(res3, NULL), NULL))

})
