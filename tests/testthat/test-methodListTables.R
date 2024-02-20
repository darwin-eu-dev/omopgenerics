test_that("listTables", {
  expect_error(listTables())
  expect_error(listTables("sdfg"))
  cdm <- emptyCdmReference("mock")
  expect_identical(listTables(cdm = cdm), character())
})
