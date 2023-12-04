#' @export
#' @importFrom dplyr arrange
arrange.tbl_lazy <- function(.data, ...) {
  dbplyr::window_order(.data, ...)
}

#' To collect a `generated_cohort_set` object.
#'
#' @param x `generated_cohort_set` object.
#' @param ... Not used (for compatibility).
#'
#' @return A data frame with yhe `generated_cohort_set`
#'
#' @export
#'
#' @importFrom dplyr collect
#'
collect.generated_cohort_set <- function(x, ...) {
  removeClass(x) <- "generated_cohort_set"
  y <- x |> dplyr::collect()
  attr(y, "cohort_set") <- attr(x, "cohort_set") |> dplyr::collect()
  attr(y, "cohort_attrition") <- attr(x, "cohort_attrition") |> dplyr::collect()
  addClass(y) <- "generated_cohort_set"
  return(y)
}
