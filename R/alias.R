# File created using OMOPUtilities::useAlias


#' @rdname assertCharacter
#' @export
assert_character <- function(x,
                             length = NULL,
                             na = FALSE,
                             null = FALSE,
                             named = FALSE,
                             min_num_character = 0,
                             call = parent.frame()) {
  assertCharacter(
    x = x,
    length = length,
    na = na,
    null = null,
    named = named,
    minNumCharacter = min_num_character,
    call = call
  )
}

#' @rdname assertChoice
#' @export
assert_choice <- function(x,
                          choices,
                          length = NULL,
                          na = FALSE,
                          null = FALSE,
                          named = FALSE,
                          call = parent.frame()) {
  assertChoice(
    x = x,
    choices = choices,
    length = length,
    na = na,
    null = null,
    named = named,
    call = call
  )
}

#' @rdname assertList
#' @export
assert_list <- function(x,
                        length = NULL,
                        na = FALSE,
                        null = FALSE,
                        named = FALSE,
                        class = NULL,
                        call = parent.frame()) {
  assertList(
    x = x,
    length = length,
    na = na,
    null = null,
    named = named,
    class = class,
    call = call
  )
}

#' @rdname assertLogical
#' @export
assert_logical <- function(x,
                           length = NULL,
                           na = FALSE,
                           null = FALSE,
                           named = FALSE,
                           call = parent.frame()) {
  assertLogical(
    x = x,
    length = length,
    na = na,
    null = null,
    named = named,
    call = call
  )
}

#' @rdname assertNumeric
#' @export
assert_numeric <- function(x,
                           integerish = FALSE,
                           min = -Inf,
                           max = Inf,
                           length = NULL,
                           na = FALSE,
                           null = FALSE,
                           named = FALSE,
                           call = parent.frame()) {
  assertNumeric(
    x = x,
    integerish = integerish,
    min = min,
    max = max,
    length = length,
    na = na,
    null = null,
    named = named,
    call = call
  )
}

#' @rdname assertTibble
#' @export
assert_tibble <- function(x,
                          number_columns = NULL,
                          number_rows = NULL,
                          columns = NULL,
                          null = FALSE,
                          call = parent.frame()) {
  assertTibble(
    x = x,
    numberColumns = number_columns,
    numberRows = number_rows,
    columns = columns,
    null = null,
    call = call
  )
}

#' @rdname cdmName
#' @export
cdm_name <- function(cdm) {
  cdmName(cdm = cdm)
}

#' @rdname checkInput
#' @export
check_input <- function(...,
                        options = list(),
                        call = parent.frame()) {
  checkInput(
    ... = ...,
    .options = options,
    call = call
  )
}

#' @rdname cohortAttrition
#' @export
cohort_attrition <- function(cohort) {
  cohortAttrition(cohort = cohort)
}

#' @rdname cohortCount
#' @export
cohort_count <- function(cohort) {
  cohortCount(cohort = cohort)
}

#' @rdname cohortSet
#' @export
cohort_set <- function(cohort) {
  cohortSet(cohort = cohort)
}

#' @rdname exportResults
#' @export
export_results <- function(...,
                           path = here::here(),
                           results_stem = "results",
                           study_id = NULL,
                           zip = TRUE) {
  exportResults(
    ... = ...,
    path = path,
    resultsStem = results_stem,
    studyId = study_id,
    zip = zip
  )
}

#' @rdname linkLogger
#' @export
link_logger <- function(cdm,
                        file) {
  linkLogger(
    cdm = cdm,
    file = file
  )
}

#' @rdname listInputCheck
#' @export
list_input_check <- function() {
  listInputCheck()
}

#' @rdname newCdmReference
#' @export
new_cdm_reference <- function(cdm_tables,
                              cdm_name,
                              cdm_version) {
  newCdmReference(
    cdmTables = cdm_tables,
    cdmName = cdm_name,
    cdmVersion = cdm_version
  )
}

#' @rdname newOmopCohort
#' @export
new_omop_cohort <- function(cohort_table,
                            cohort_set_table = NULL,
                            cohort_attrition_table = NULL,
                            cohort_count_table = NULL,
                            cohort_name = "cohort") {
  newOmopCohort(
    cohortTable = cohort_table,
    cohortSetTable = cohort_set_table,
    cohortAttritionTable = cohort_attrition_table,
    cohortCountTable = cohort_count_table,
    cohortName = cohort_name
  )
}

#' @rdname toCamelCase
#' @export
to_camel_case <- function(string) {
  toCamelCase(string = string)
}

#' @rdname toSnakeCase
#' @export
to_snake_case <- function(string) {
  toSnakeCase(string = string)
}

#' @rdname validateCdmCohort
#' @export
validate_cdm_cohort <- function(cohort_table) {
  validateCdmCohort(cohortTable = cohort_table)
}

#' @rdname validateCdmReference
#' @export
validate_cdm_reference <- function(cdm) {
  validateCdmReference(cdm = cdm)
}

#' @rdname writeLogger
#' @export
write_logger <- function(cdm,
                         message,
                         warn = FALSE) {
  writeLogger(
    cdm = cdm,
    message = message,
    warn = warn
  )
}
