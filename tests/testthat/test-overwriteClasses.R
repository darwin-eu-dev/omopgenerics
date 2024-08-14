test_that("test that classes and attributes are keep", {

  # add class and attributes
  x <- dplyr::tibble(a = 1) |>
    newCdmTable(src = newLocalSource(), name = "xuxudecrema")

  # nothing
  funs <- c("collapse", "count", "rowwise", "tally", "ungroup")
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x)"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # a column
  funs <- c("group_by")
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x, a)"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # join functions
  funs <- c(
    "anti_join", "full_join", "inner_join", "left_join", "nest_join",
    "right_join",  "semi_join"
  )
  y <- dplyr::tibble(a = 1:5)
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x, y, by = 'a')"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # no by
  funs <- c("cross_join", "union", "union_all")
  for (fun in funs) {
    expect_no_error(eval(parse(text = paste0("xn <- dplyr::", fun, "(x, y)"))))
    expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
    expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
    expect_true("cdm_table" %in% class(xn))
  }

  # summarise
  expect_no_error(xn <- x |> dplyr::summarise(n = dplyr::n()))
  expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
  expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
  expect_true("cdm_table" %in% class(xn))

  # group_by + mutate
  person <- dplyr::tibble(
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2023-12-31"),
    period_type_concept_id = 0L
  )
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1,2) |> as.integer(),
    subject_id = c(1,1) |> as.integer(),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  cdm <- cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cohortTables = list("cohort1" = cohort1),
    cdmName = "test"
  )

  cl <- cdm$cohort1 |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(a = 1) |>
    class()

  expect_true(all(c("cohort_table", "cdm_table") %in% cl))

})

