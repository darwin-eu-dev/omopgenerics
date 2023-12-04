#' Get settings from an object.
#'
#' @param x x
#'
#' @return A table with the details of the cohort set.
#'
#' @export
set <- function(x) {
  UseMethod("set")
}

#' Get cohort settings from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the details of the cohort set.
#'
#' @export
set.generated_cohort_set <- function(cohort) {
  if (is.null(attr(cohort, "cohort_set"))) {
    cli::cli_abort("Cohort set does not exist for this cohort.")
  }
  attr(cohort, "cohort_set") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id)
}
