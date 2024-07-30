
test_that("test validateNameArgument", {
  expect_error(validateNameArgument(name = 1))
  expect_error(validateNameArgument(name = c("sda", "asdfsa")))
  expect_identical("my_name", validateNameArgument("my_name"))
  expect_error(validateNameArgument("myName"))
  expect_warning(expect_identical("my_name", validateNameArgument("myName", validation = "warning")))
  expect_warning(expect_warning(expect_identical(
    "my_name", validateNameArgument("myName", list("my_name" = 1), validation = "warning")
  )))
})

test_that("test validateCohortIdArgument", {
  cohort <- 1
  class(cohort) <- c("cohort_table", "cdm_table")
  attr(cohort, "cohort_set") <- dplyr::tibble(
    "cohort_definition_id" = c(2, 4), "cohort_name" = c("a", "b")
  )
  expect_error(validateCohortIdArgument("adsfd", cohort))
  expect_identical(validateCohortIdArgument(2, cohort), 2L)
  expect_identical(validateCohortIdArgument(c(2, 4), cohort), c(2L, 4L))
  expect_identical(validateCohortIdArgument(c(4L, 2L), cohort), c(4L, 2L))
  expect_error(validateCohortIdArgument(1, cohort))
})



test_that("test validateAgeGroup", {
  #test list
  ageGroup = c(0, 18)

  expect_error(validateAgeGroupArgument(ageGroup))
  ageGroup = list(c(0, 18))
  expect_no_error(validateAgeGroupArgument(ageGroup))

  #test overlap
  ageGroup = list(c(0, 18), c(16, 20))
  expect_error(validateAgeGroupArgument(ageGroup, overlap = FALSE))
  expect_no_error(validateAgeGroupArgument(ageGroup, overlap = TRUE))

  #test order
  ageGroup = list(c(19, 18), c(21, 20))
  expect_error(validateAgeGroupArgument(ageGroup, overlap = FALSE))



})
