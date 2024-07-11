test_that("test cdm_reference", {
  src <- newLocalSource()
  cdmTables <- list(
    "person" = dplyr::tibble(
      person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
      race_concept_id = 0L, ethnicity_concept_id = 0L
    ) |>
      newCdmTable(src, "person"),
    "observation_period" = dplyr::tibble(
      observation_period_id = 1L, person_id = 1L,
      observation_period_start_date = as.Date("2000-01-01"),
      observation_period_end_date = as.Date("2025-12-31"),
      period_type_concept_id = 0L
    ) |>
      newCdmTable(src, "observation_period")
  )

  expect_no_error(cdm <- newCdmReference(tables = cdmTables, cdmName = "mock"))

  expect_error(cdm[["not_present"]])
  expect_no_error(cdm[["person"]])
  expect_no_error(cdm[["observation_period"]])
  expect_identical(cdm[[1]], cdm[["person"]])
  expect_true(is.null(cdm[[3]]))

  expect_identical(cdm, cdmReference(cdm$person))

  expect_identical("mock", cdmName(cdm))
  expect_identical(src, cdmSource(cdm))
  expect_true("cdm_reference" %in% names(attributes(cdm$person)))
  expect_true("cdm_reference" %in% names(attributes(cdm[["person"]])))

  expect_error(newCdmReference(tables = cdmTables["person"], cdmName = "mock"))

  expect_error(newCdmReference(tables = list(), cdmName = "mock"))

  cdmUpper <- cdmTables
  names(cdmUpper) <- toupper(names(cdmUpper))
  expect_error(newCdmReference(tables = cdmUpper, cdmName = "mock"))

  cdmTables$person <- cdmTables$person |>
    dplyr::rename("PERSON_ID" = "person_id")
  expect_error(newCdmReference(tables = cdmTables, cdmName = "mock"))

  expect_error(
    dplyr::tibble(
      PERSON_ID = 1, gender_concept_id = 0, year_of_birth = 1990,
      race_concept_id = 0, ethnicity_concept_id = 0
    ) |>
      newCdmTable(src, "person")
  )

  cdmTables$person <- cdmTables$person |>
    dplyr::select(-"PERSON_ID")
  expect_error(newCdmReference(tables = cdmTables, cdmName = "mock"))

})

test_that("test assign and extract from cdm object", {
  x <- list(a = 1, b = 2)
  class(x) <- "cdm_reference"
  expect_true("cdm_reference" %in% names(attributes(x[["a"]])))
  expect_true("cdm_reference" %in% names(attributes(x$a)))

  # if I do it for an object that it is not a cdm_reference it wont
  xu <- unclass(x)
  expect_false("cdm_reference" %in% names(attributes(xu[["a"]])))
  expect_false("cdm_reference" %in% names(attributes(xu$a)))

  # I define an element with a cdm reference on it
  xx <- 3
  attr(xx, "tbl_name") <- "c"
  class(xx) <- "cdm_table"
  attr(xx, "cdm_reference") <- 4
  expect_true("cdm_reference" %in% names(attributes(xx)))

  # if I assign this element to a list with no class the attribute will persist
  xu$c <- xx
  expect_true("cdm_reference" %in% names(attributes(xu[["c"]])))
  expect_true("cdm_reference" %in% names(attributes(xu$c)))

  # if I assign to a cdm_reference it wont but it will appear back when I access
  # to one of the elements
  expect_no_error(x$c <- xx)
  expect_true("cdm_reference" %in% names(attributes(x[["c"]])))
  expect_true("cdm_reference" %in% names(attributes(x$c)))

  # but if after assigning I remove the class the attribute wont be there
  # because when I assigned it was eliminated
  xuu <- unclass(x)
  expect_false("cdm_reference" %in% names(attributes(xuu[["c"]])))
  expect_false("cdm_reference" %in% names(attributes(xuu$c)))

  # summary empty cdm
  expect_no_warning(summary(emptyCdmReference("test")))
})
