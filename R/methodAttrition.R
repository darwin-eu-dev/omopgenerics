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
#' @param x A generated_cohort_set object.
#'
#' @return A table with the attrition.
#'
#' @export
attrition.generated_cohort_set <- function(x) {
  if (is.null(attr(x, "cohort_attrition"))) {
    cli::cli_abort("Cohort attrition does not exist for this cohort.")
  }
  attr(x, "cohort_attrition") |>
    dplyr::collect() |>
    dplyr::arrange(.data$cohort_definition_id, .data$reason_id) |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id),
                  number_records = as.integer(.data$number_records),
                  number_subjects = as.integer(.data$number_subjects),
                  reason_id = as.integer(.data$reason_id),
                  reason = as.character(.data$reason),
                  excluded_records = as.integer(.data$excluded_records),
                  excluded_subjects = as.integer(.data$excluded_subjects))
}
