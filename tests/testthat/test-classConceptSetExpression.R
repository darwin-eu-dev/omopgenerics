test_that("simple examples of concept set", {

asthma_cs <- list("asthma_narrow" = dplyr::tibble(
    "concept_id" = 1,
    "excluded" = FALSE,
    "descendants" = TRUE,
    "mapped" = FALSE
  ),
  "asthma_broad" = dplyr::tibble(
    "concept_id" = c(1,2),
    "excluded" = FALSE,
    "descendants" = TRUE,
    "mapped" = FALSE
  ))
expect_no_error(asthma_cs <- newConceptSetExpression(asthma_cs))

# no error if extra columns
asthma_cs <- list("asthma_narrow" = dplyr::tibble(
  "concept_id" = 1,
  "concept_name" = "asthma",
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
),
"asthma_broad" = dplyr::tibble(
  "concept_id" = c(1,2),
  "concept_name" = "asthma",
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
))

expect_no_error(asthma_cs <- newConceptSetExpression(asthma_cs))

# expected errors
expect_error(newConceptSetExpression(list("asthma_narrow" = dplyr::tibble(
  "concept_id" = 1,
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
),
dplyr::tibble( # no name
  "concept_id" = 2,
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
))))

expect_error(newConceptSetExpression(list("asthma_narrow" = dplyr::tibble(
  "concept_id" = 1,
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
),
"asthma_broad" = dplyr::tibble(
  "concept_id" = "not a id",
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
))))

expect_error(newConceptSetExpression(list("asthma_narrow" = dplyr::tibble(
  "concept_id" = 1,
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
),
"asthma_broad" = dplyr::tibble(
  "concept_id" = 2,
  "excluded" = "not logical",
  "descendants" = TRUE,
  "mapped" = FALSE
))))

expect_error(newConceptSetExpression(list("asthma_narrow" = dplyr::tibble(
  "concept_id" = 1,
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
),
"asthma_broad" = dplyr::tibble(
  "concept_id" = 2,
  "excluded" = TRUE,
  "descendants" = "not logical",
  "mapped" = FALSE
))))

expect_error(newConceptSetExpression(list("asthma_narrow" = dplyr::tibble(
  "concept_id" = 1,
  "excluded" = FALSE,
  "descendants" = TRUE,
  "mapped" = FALSE
),
"asthma_broad" = dplyr::tibble(
  "concept_id" = 2,
  "excluded" = TRUE,
  "descendants" = TRUE,
  "mapped" = "not logical"
))))

expect_error(newConceptSetExpression(list(
  "diabetes" = dplyr::tibble(
    "concept_id" = c(201820, NA),
    "excluded" = c(FALSE, FALSE),
    "descendants" = c(TRUE, FALSE),
    "mapped" = c(FALSE, FALSE)
  ),
  "asthma" = dplyr::tibble(
    "concept_id" = 317009,
    "excluded" = FALSE,
    "descendants" = FALSE,
    "mapped" = FALSE
  )
)))


})
