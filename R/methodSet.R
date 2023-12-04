#' Get cohort settings from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet <- function(cohort) {
  UseMethod("cohortSet")
}

#' Get cohort settings from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
cohortSet.generated_cohort_set <- function(cohort) {
  if (is.null(attr(cohort, "cohort_set"))) {
    cli::cli_abort("Cohort set does not exist for this cohort.")
  }
  attr(cohort, "cohort_set") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id)
}
