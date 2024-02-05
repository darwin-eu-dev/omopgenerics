test_that("prefix", {
  # unique table name
  options(dbplyr_table_name = 0)
  expect_true(uniqueTableName() == "dbplyr_001")
  expect_true(uniqueTableName() == "dbplyr_002")
  options(dbplyr_table_name = 100)
  expect_true(uniqueTableName() == "dbplyr_101")

  # unique prefix
  expect_true(randomPrefix(n = 0) == "_")
  expect_true(nchar(randomPrefix(n = 5)) == 6)
  x <- randomPrefix(n = 3) |> strsplit(split = "") |> unlist()
  expect_true(length(x) == 4)
  expect_true(x[1] %in% letters)
  expect_true(x[2] %in% letters)
  expect_true(x[3] %in% letters)
  expect_true(x[4] == "_")

  # table with prefix
  expect_no_error(x <- uniqueTableName(randomPrefix()))
  expect_true(nchar(x) == 16)
  x <- x |> strsplit(split = "_") |> unlist()
  expect_true(length(x) == 3)
  expect_true(nchar(x[1]) == 5)
  expect_true(all(strsplit(x = x[1], split = "") |> unlist() %in% letters))
  expect_true(x[2] == "dbplyr")
  expect_true(x[3] == "102")
})
