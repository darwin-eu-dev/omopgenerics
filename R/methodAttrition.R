#' Get attrition from an object.
#'
#' @param x x.
#'
#' @return A table with the attrition.
#'
#' @export
attrition <- function(x) {
  UseMethod("attrition")
}

#' Get cohort attrition from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the attrition.
#'
#' @export
attrition.generated_cohort_set <- function(cohort) {
  if (is.null(attr(cohort, "cohort_attrition"))) {
    cli::cli_abort("Cohort attrition does not exist for this cohort.")
  }
  attr(cohort, "cohort_attrition") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id)
}
