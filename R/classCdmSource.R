#' Create a cdm source object.
#'
#' @param src Source to a cdm object.
#' @param sourceName Name of the source.
#' @param sourceType Type of the source object.
#'
#' @export
#'
#' @return A validated cdm source object.
#'
cdmSource <- function(src, sourceName, sourceType) {
  # initial check
  assertCharacter(sourceName, length = 1, minNumCharacter = 1)
  assertCharacter(sourceType, length = 1, minNumCharacter = 1)

  # assign class
  src <- newCdmSource(src = src, sourceName = sourceName, sourceType = sourceType)

  # validate source
  src <- validateCdmSource(src = src)

  return(src)
}

newCdmSource <- function(src, sourceName, sourceType) {
  addClass(src) <- "cdm_source"
  attr(src, "source_name") <- sourceName
  attr(src, "source_type") <- sourceType
  return(src)
}
validateCdmSource <- function(src) {
  # toy data
  name <- paste0(c(sample(letters, 5, replace = TRUE), "_test_table"), collapse = "")
  table <- datasets::cars

  # create mock cdm
  cdm <- cdmReference(
    cdmTables = list(
      person = dplyr::tibble(
        person_id = 1, gender_concept_id = 0, year_of_birth = 2000,
        race_concept_id = 0, ethnicity_concept_id = 0
      ),
      observation_period = dplyr::tibble(
        observation_period_id = 1, person_id = 1,
        observation_period_start_date = as.Date("2010-01-01"),
        observation_period_end_date = as.Date("2029-12-31"),
        period_type_concept_id = 0
      )
    ),
    cdmName = "mock",
    cdmSource = src
  )

  # insert table
  cdm[[name]] <- insertTable(src = cdm, name = name, table = table)
  validateX(x = cdm[[name]], name = name, fun = "insertTable")

  # check inserted table
  attr(table, "tbl_name") <- name
  attr(table, "cdm_reference") <- cdm
  if (!identical(unclass(dplyr::collect(cdm[[name]])), unclass(table))) {
    cli::cli_abort("The inserted table was not the same than the provided one.")
  }

  # compute inserted table
  cdm[[name]] <- cdm[[name]] |> computeTable(cdm = cdm, name = name)
  validateX(x = cdm[[name]], name = name, fun = "computeTable")

  # drop table
  cdm <- dropTable(src = cdm, name = name)

  # check that a cohort can be inserted and computed
  cdm[[name]] <- insertTable(src = cdm, name = name, table = dplyr::tibble(
    "cohort_definition_id" = 1,
    "subject_id" = 1,
    "cohort_start_date" = as.Date("2020-01-01"),
    "cohort_end_date" = as.Date("2020-01-01")
  ))
  value <- generatedCohortSet(cohortRef = cdm[[name]]) # it will be a cohort without a source
  # insert table
  x <- insertTable(src = src, name = name, table = value)
  validateX(x = x, name = name, fun = "insertTable")

  # check inserted table
  if (!identical(unclass(dplyr::collect(x)), unclass(value))) {
    cli::cli_abort("The inserted table was not the same than the provided one.")
  }

  # compute inserted table
  x <- x |> computeTable(cdm = cdm, name = name)
  validateX(x = x, name = name, fun = "computeTable")

  # drop table
  dropTable(src = src, name = name)

  return(invisible(src))
}

validateX <- function(x, name, fun) {
  if (!identical(attr(x, "tbl_name"), name)) {
    cli::cli_abort("table name is not correctly assigned in {fun}")
  }
  # if (!"cdm_table" %in% class(x)) {
  #   cli::cli_abort("cdm_table class is not correctly assigned in {fun}")
  # }
  return(invisible(TRUE))
}

#' @export
print.cdm_source <- function(x, ...) {
  cli::cli_inform(
    "This is a {attr(x, 'source_type')} cdm source of {attr(x, 'source_name')}"
  )
}
