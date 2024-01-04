test_that("bind no class", {
  x <- 1
  expect_error(bind(x))
})

test_that("bind a cohort_set", {
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
        person_id = c(1, 2, 3), gender_concept_id = 0, year_of_birth = 1990,
        race_concept_id = 0, ethnicity_concept_id = 0
      ),
      "observation_period" = dplyr::tibble(
        observation_period_id = 1:3, person_id = 1:3,
        observation_period_start_date = as.Date("2000-01-01"),
        observation_period_end_date = as.Date("2025-12-31"),
        period_type_concept_id = 0
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
  expect_error(bind(cdm$cohort1, name = "new_cohort"))
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
})

