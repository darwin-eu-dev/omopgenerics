test_that("test that classes and attributes are keep", {
  # group_by
  x <- addClassAndAttributes(dplyr::tibble(a = 1))
  expect_no_error(xn <- x |> dplyr::group_by(.data$a))
  expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
  expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
  expect_true("cdm_table" %in% class(xn))

  # summarise
  x <- addClassAndAttributes(dplyr::tibble(a = 1))
  expect_no_error(xn <- x |> dplyr::summarise(n = dplyr::n()))
  expect_identical(attr(x, "cdm_reference"), attr(xn, "cdm_reference"))
  expect_identical(attr(x, "tbl_name"), attr(xn, "tbl_name"))
  expect_true("cdm_table" %in% class(xn))
})
