test_that("listSourceTables", {
  expect_error(listSourceTables())
  expect_error(listSourceTables("sdfg"))
  cdm <- emptyCdmReference("mock")
  expect_identical(listSourceTables(cdm = cdm), character())
})
