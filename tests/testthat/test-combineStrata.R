test_that("test combineStrata", {
  expect_identical(combineStrata(c("age_group", "sex", "year")),
                list(
                  "age_group",
                  "sex",
                  "year",
                  c("age_group","sex"),
                  c("age_group","year"),
                  c("sex","year"),
                  c("age_group","sex","year")
                )
  )

  # Expected errors and results for edge cases
  expect_identical(combineStrata(c("sex")), list("sex"))
  expect_error(combineStrata(sex))
  expect_error(combineStrata(c(2,3)))
  expect_identical(
    combineStrata(c("sex", NA)), list("sex", NA_character_, c("sex", NA))
  )
  expect_identical(combineStrata(NULL), list())
  expect_identical(combineStrata(c("")), list(""))
  expect_identical(combineStrata(c("")[-1]), list())
})
