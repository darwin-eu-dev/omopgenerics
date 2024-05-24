
test_that("test validateName", {
  expect_error(validateName(name = 1))
  expect_error(validateName(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateName("my_name"))
  expect_message(expect_identical("my_name", validateName("myName")))
  expect_message(expect_message(expect_identical(
    "my_name", validateName("myName", list("my_name" = 1))
  )))
})
