test_that("prefix", {
  # unique table name
  options(dbplyr_table_name = 0)
  expect_true(stringr::str_starts(uniqueTableName(), "og_001"))
  expect_true(stringr::str_starts(uniqueTableName(),"og_002"))
  options(dbplyr_table_name = 100)
  expect_true(stringr::str_starts(uniqueTableName(), "og_101"))

  # temporary prefix
  options(tmp_prefix_number = 0)
  expect_true(tmpPrefix() == "tmp_001_")
  expect_true(tmpPrefix() == "tmp_002_")
  expect_true(getOption("tmp_prefix_number") == 2)
  options(tmp_prefix_number = 123)
  expect_true(tmpPrefix() == "tmp_124_")
  expect_true(getOption("tmp_prefix_number") == 124)

  # table with prefix
  options(tmp_prefix_number = 111)
  expect_no_error(x <- uniqueTableName(tmpPrefix()))
  expect_true(nchar(x) == 25)
  x <- x |> strsplit(split = "_") |> unlist()
  expect_true(length(x) == 5)
  expect_true(x[1] == "tmp")
  expect_true(x[2] == "112")
  expect_true(x[3] == "og")
  expect_true(x[4] == "102")
})
