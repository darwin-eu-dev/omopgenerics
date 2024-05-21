
test_that("test assertCharacter", {
  # class
  expect_error(assertCharacter(1))
  expect_no_error(assertCharacter("xcz"))

  # minimum characters
  expect_error(assertCharacter("xcz", minNumCharacter = 4))
  expect_error(assertCharacter(c("", "asdfsgdg", "xcz", "dafcdfgdv"), minNumCharacter = 4))
  expect_error(assertCharacter(c("", NA, "asdfsgdg", "xcz", "dafcdfgdv"), minNumCharacter = 4, na = TRUE))

  # null
  expect_no_error(assertCharacter("xcz", null = TRUE))
  expect_no_error(assertCharacter(c("xcz", "adsddaxcsdc"), null = TRUE))
  expect_no_error(assertCharacter("xcz", null = FALSE))
  expect_no_error(assertCharacter(NULL, null = TRUE))
  expect_error(assertCharacter(NULL, null = FALSE))

  # length
  expect_error(assertCharacter(c("affac", "sadf", "asdsef"), length = 2))
  expect_no_error(assertCharacter(c("affac", "sadf", "asdsef"), length = 3))
  expect_error(assertCharacter(c("affac", "sadf", "asdsef"), length = 4))

  # na
  expect_error(assertCharacter(c("affac", "sadf", "asdsef", NA), na = FALSE))
  expect_error(assertCharacter(c("affac", NA, NA, "sadf", "asdsef", NA), na = FALSE))
  expect_no_error(assertCharacter(c("affac", "sadf", "asdsef", NA), na = TRUE))
  expect_no_error(assertCharacter(c("affac", NA, NA, "sadf", "asdsef", NA), na = TRUE))

  # unique
  expect_no_error(assertCharacter(c("affac", "sadf", "asdsef"), unique = TRUE))
  expect_error(assertCharacter(c("affac", NA, NA, "sadf", "asdsef", NA), na = TRUE, unique = TRUE))
  expect_no_error(assertCharacter(c("affac", "sadf", "asdsef"), unique = FALSE))
  expect_no_error(assertCharacter(c("affac", NA, NA, "sadf", "asdsef", NA), na = TRUE, unique = FALSE))
  expect_error(assertCharacter(c("affac", "sadf", "sadf", "asdsef"), unique = TRUE))
  expect_error(assertCharacter(c("affac", NA, NA, "sadf", "sadf", "asdsef", NA), na = TRUE, unique = TRUE))
  expect_no_error(assertCharacter(c("affac", "sadf", "sadf", "asdsef"), unique = FALSE))
  expect_no_error(assertCharacter(c("affac", NA, NA, "sadf", "sadf", "asdsef", NA), na = TRUE, unique = FALSE))

  # named
  expect_no_error(assertCharacter(c("asd" = "sadsf", "ascv scvd"), named = FALSE))
  expect_error(assertCharacter(c("asd" = "sadsf", "ascv scvd"), named = TRUE))
  expect_no_error(assertCharacter(c("asd" = "sadsf", "sad" = "ascv scvd"), named = TRUE))
  expect_error(assertCharacter(c("asd" = "sadsf", "sad" = "ascv scvd", NA), named = TRUE))
  expect_no_error(assertCharacter(c("asd" = "sadsf", "sad" = "ascv scvd", "asd" = NA), named = TRUE, na = TRUE))

  # msg
  expect_error(assertCharacter(c("affac", "sadf", "asdsef"), length = 2, msg = "my custom message"))
  expect_error(assertCharacter(c("affac", NA, NA, "sadf", "sadf", "asdsef", NA), na = TRUE, unique = TRUE, msg = character()))
})
