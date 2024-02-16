test_that("prefix", {
  # unique table name
  options(dbplyr_table_name = 0)
  expect_true(uniqueTableName() == "dbplyr_001")
  expect_true(uniqueTableName() == "dbplyr_002")
  options(dbplyr_table_name = 100)
  expect_true(uniqueTableName() == "dbplyr_101")

  # temporary prefix
  options(tmp_prefix_number = 0)
  expect_true(tmpPrefix() == "tmp_001_")
  expect_true(tmpPrefix() == "tmp_002_")
  expect_true(getOption("tmp_prefix_number") == 2)
  options(tmp_prefix_number = 123)
  expect_true(tmpPrefix() == "tmp_124_")
  expect_true(getOption("tmp_prefix_number") == 124)

  # table with prefix
  expect_no_error(x <- uniqueTableName(tmpPrefix()))
  expect_true(nchar(x) == 18)
  x <- x |> strsplit(split = "_") |> unlist()
  expect_true(length(x) == 4)
  expect_true(x[1] == "tmp")
  expect_true(x[2] == "125")
  expect_true(x[3] == "dbplyr")
  expect_true(x[4] == "102")
})
