test_that("test cdm_reference", {

  # warning
  expect_no_error(x <- cdmReference(
    cdmTables = list("person" = dplyr::tibble(a = 1))
  ))

  # no warning
  expect_no_error(x <- cdmReference(
    cdmTables = list("person" = 1)
  ))

  # no warning
  expect_no_error(x <- cdmReference(
    cdmTables = list(list("person" = 1))
  ))

  # no warning
  expect_no_error(x <- cdmReference(
    cdmTables = list("person" = unclass(dplyr::tibble(a = 1)))
  ))

})
