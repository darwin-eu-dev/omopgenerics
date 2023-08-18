test_that("export a cdm object", {
  #cdm <- mockCdm()
  #expect_no_error(cdmSnapshot <- export(cdm))
})

test_that("export a cohort object", {
  #cdm <- mockCdm()
  #expect_no_error(cohort <- export(cdm$cohort1))
})

test_that("export a tibble", {
  x <- dplyr::tibble(a = 1)
  expect_no_error(xn <- export(x))
  expect_identical(x, xn)
})

test_that("expect error for random class", {
  x <- 1
  class(x) <- "random"
  expect_error(export(x))
})

test_that("export results", {
  #cdm <- mockCdm()
  #expect_no_error(exportResults(cdm, cdm$cohort1, dplyr::tibble(cdm_name = 1)))
  #expect_true()
})
