test_that("splitGroup", {
  tib <- dplyr::tibble(
    group_name = c("cohort_name", "cohort_name and age", "age and sex"),
    group_level = c("acetaminophen", "ibuprofen and 10 to 19", "20 to 29 and Male"),
    x = c("cohort_name", "cohort_name and age", "age and sex"),
    y = c("acetaminophen", "ibuprofen and 10 to 19", "20 to 29 and Male"),
    z = c(1, 2, 3),
    a = c("a", "b", "c")
  )
  expect_error(tib |> splitGroup(NA_character_))
  expect_no_error(res0 <- tib |> splitGroup())
  expect_false("group_name" %in% colnames(res0))
  expect_false("group_level" %in% colnames(res0))
  expect_true(all(c("cohort_name", "age", "sex") %in% colnames(res0)))
  expect_equal(res0$cohort_name, c("acetaminophen", "ibuprofen", NA_character_))
  expect_equal(res0$age, c(NA_character_, "10 to 19", "20 to 29"))
  expect_equal(res0$sex, c(NA_character_, NA_character_, "Male"))
  expect_no_error(res1 <- tib |> splitGroup(keep = TRUE))
  expect_true("group_name" %in% colnames(res1))
  expect_true("group_level" %in% colnames(res1))
  expect_true(all(c("cohort_name", "age", "sex") %in% colnames(res1)))
  expect_warning(tib |> splitGroup(keep = TRUE) |> splitGroup())
  expect_no_error(res2 <- tib |> splitGroup(name = "x", level = "y", keep = TRUE))
  expect_equal(res1, res2)
  expect_error(tib |> splitGroup(name = "expo_group", level = "exposure_level"))
  expect_error(tib |> splitGroup(level = NA_character_))
  expect_error(tib |> splitGroup(level = "a"))
})
