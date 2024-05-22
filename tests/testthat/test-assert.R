
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

test_that("test assertChoice", {
  # null
  expect_no_error(assertChoice("a", choices = letters, null = TRUE))
  expect_no_error(assertChoice(c("a", "f"), choices = letters, null = TRUE))
  expect_no_error(assertChoice("a", choices = letters, null = FALSE))
  expect_error(assertChoice(NULL, choices = letters, null = FALSE))
  expect_no_error(assertChoice(NULL, choices = letters, null = TRUE))

  # class
  expect_error(assertChoice(1, choices = letters))

  # length
  expect_no_error(assertChoice("a", choices = letters, length = 1))
  expect_error(assertChoice("a", choices = letters, length = 2))

  # na
  expect_no_error(assertChoice("a", choices = letters, na = TRUE))
  expect_no_error(assertChoice(c("a", NA), choices = letters, na = TRUE))
  expect_no_error(assertChoice("a", choices = letters, na = FALSE))
  expect_error(assertChoice(c("a", NA), choices = letters, na = FALSE))

  # unique
  expect_no_error(assertChoice("a", choices = letters, unique = TRUE))
  expect_error(assertChoice(c("a", "b", "a"), choices = letters, unique = TRUE))
  expect_no_error(assertChoice("a", choices = letters, unique = FALSE))
  expect_no_error(assertChoice(c("a", "b", "a"), choices = letters, unique = FALSE))

  # named
  expect_error(assertChoice(c("a"), choices = letters, named = TRUE))
  expect_error(assertChoice(c("a","xc" = "b"), choices = letters, named = TRUE))
  expect_no_error(assertChoice(c("sdaf" = "a", "xc" = "b"), choices = letters, named = TRUE))
  expect_no_error(assertChoice(c("a"), choices = letters, named = FALSE))
  expect_no_error(assertChoice(c("a","xc" = "b"), choices = letters, named = FALSE))
  expect_no_error(assertChoice(c("sdaf" = "a", "xc" = "b"), choices = letters, named = FALSE))

  # choice
  expect_no_error(assertChoice("a", choices = letters, unique = TRUE))
  expect_error(assertChoice(c("a", "b", "asdfv"), choices = letters, unique = TRUE))
})

test_that("test assertClass", {
  x1 <- 1
  class(x1) <- c("ref", "raf")
  x2 <- c(1, 1)
  class(x2) <- c("raf", "ref")
  x3 <- 1
  class(x3) <- "ref"

  # class
  expect_no_error(assertClass(x1, "ref"))
  expect_error(assertClass(x1, "rof"))

  # length
  expect_no_error(assertClass(x1, "ref", length = 1))
  expect_error(assertClass(x2, "ref", length = 1))
  expect_error(assertClass(x1, "ref", length = 2))
  expect_no_error(assertClass(x2, "ref", length = 2))

  # null
  expect_no_error(assertClass(NULL, "ref", null = TRUE))
  expect_error(assertClass(NULL, "ref", null = FALSE))

  # all and extra
  expect_no_error(assertClass(x1, "ref"))
  expect_no_error(assertClass(x2, "ref"))
  expect_no_error(assertClass(x3, "ref"))
  expect_no_error(assertClass(x1, c("ref", "raf"), all = TRUE, extra = TRUE))
  expect_no_error(assertClass(x2, c("ref", "raf"), all = TRUE, extra = TRUE))
  expect_error(assertClass(x3, c("ref", "raf"), all = TRUE, extra = TRUE))
  expect_no_error(assertClass(x1, c("ref", "raf"), all = FALSE, extra = TRUE))
  expect_no_error(assertClass(x2, c("ref", "raf"), all = FALSE, extra = TRUE))
  expect_no_error(assertClass(x3, c("ref", "raf"), all = FALSE, extra = TRUE))
  expect_no_error(assertClass(x1, c("ref", "raf"), all = TRUE, extra = FALSE))
  expect_no_error(assertClass(x2, c("ref", "raf"), all = TRUE, extra = FALSE))
  expect_error(assertClass(x3, c("ref", "raf"), all = TRUE, extra = FALSE))
  expect_error(assertClass(x1, "ref", all = FALSE, extra = FALSE))
  expect_error(assertClass(x2, "ref", all = FALSE, extra = FALSE))
  expect_no_error(assertClass(x3, "ref", all = FALSE, extra = FALSE))

})

test_that("test assertList", {
  # not list
  expect_no_error(assertList(list()))
  expect_error(assertList(1))

  # length
  expect_no_error(assertList(list(1, "wd"), length = 2))
  expect_error(assertList(list(1, "wd"), length = 3))

  # na
  expect_no_error(assertList(list(1, NA, "ws"), na = TRUE))
  expect_error(assertList(list(1, NA, "ws"), na = FALSE))
  expect_no_error(assertList(list(c(1, NA), 2, "ws"), na = FALSE))

  # null
  expect_no_error(assertList(NULL, null = TRUE))
  expect_no_error(assertList(list(1, "wd"), null = TRUE))
  expect_error(assertList(NULL, null = FALSE))
  expect_no_error(assertList(list(1, "wd"), null = FALSE))

  # unique
  expect_error(assertList(list(1, 2, NA, 2), unique = TRUE, na = TRUE))
  expect_no_error(assertList(list(1, 2, NA, 2), unique = FALSE, na = TRUE))
  expect_error(assertList(list(1, 2, NA, NA), unique = TRUE, na = TRUE))
  expect_no_error(assertList(list(1, 2, NA, NA), unique = FALSE, na = TRUE))

  # named
  expect_error(assertList(list("sa"= 1, 2), named = TRUE))
  expect_no_error(assertList(list("sa"= 1, "xzc" = 2), named = TRUE))
  expect_no_error(assertList(list("sa"= 1, 2), named = FALSE))
  expect_no_error(assertList(list("sa"= 1, "xzc" = 2), named = FALSE))

  # class
  expect_no_error(assertList(list(1, 2), class = "numeric"))
  expect_error(assertList(list(1, 2), class = "character"))
  expect_no_error(assertList(list(1, 2), class = c("character", "numeric")))
  expect_error(assertList(list("1", "2"), class = "numeric"))
  expect_no_error(assertList(list("1", "2"), class = "character"))
  expect_no_error(assertList(list("1", "2"), class = c("character", "numeric")))
  expect_error(assertList(list(1, "2"), class = "numeric"))
  expect_error(assertList(list(1, "2"), class = "character"))
  expect_no_error(assertList(list(1, "2"), class = c("character", "numeric")))
})

test_that("test assertLogical", {
  # not logical
  expect_no_error(assertLogical(TRUE))
  expect_error(assertLogical(1))

  # length
  expect_error(assertLogical(TRUE, length = 5))
  expect_no_error(assertLogical(c(T, T, F, F ,F), length = 5))

  # na
  expect_no_error(assertLogical(c(TRUE, NA), na = TRUE))
  expect_no_error(assertLogical(c(TRUE, F), na = TRUE))
  expect_error(assertLogical(c(TRUE, NA), na = FALSE))
  expect_no_error(assertLogical(c(TRUE, F), na = FALSE))

  # null
  expect_no_error(assertLogical(NULL, null = TRUE))
  expect_error(assertLogical(NULL, null = FALSE))
  expect_no_error(assertLogical(TRUE, null = TRUE))
  expect_no_error(assertLogical(TRUE, null = FALSE))

  # named
  expect_no_error(assertLogical(c("qwd" = TRUE), named = TRUE))
  expect_error(assertLogical(TRUE, named = TRUE))
  expect_error(assertLogical(c("qwd" = TRUE, FALSE), named = TRUE))
  expect_no_error(assertLogical(c("qwd" = TRUE, "saz" = FALSE), named = TRUE))
})

test_that("test assertNumeric", {
  # not numeric

  # integerish

  # min

  # max

  # length

  # na

  # null

  # unique

  # named

})

test_that("test assertTable", {

})
