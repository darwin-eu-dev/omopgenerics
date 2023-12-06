#' Get cohort counts from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount <- function(cohort) {
  UseMethod("cohortCount")
}

#' Get cohort counts from a generated_cohort_set object.
#'
#' @param cohort A generated_cohort_set object.
#'
#' @return A table with the counts.
#'
#' @export
cohortCount.generated_cohort_set <- function(cohort) {
  if (is.null(attr(cohort, "cohort_attrition"))) {
    cli::cli_abort("Cohort count does not exist for this cohort.")
  }
  attr(cohort, "cohort_attrition") |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::filter(.data$reason_id == max(.data$reason_id, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(
      "cohort_definition_id", "number_records", "number_subjects"
    ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  number_records = as.integer(.data$number_records),
                  number_subjects = as.integer(.data$number_subjects))
}
