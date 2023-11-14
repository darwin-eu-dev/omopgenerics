
#' Compute a cdm_table
#'
#' @param x Table to compute.
#' @param name Name of the table.
#' @param ... For compatibility.
#'
#' @return The cdm_table computed
#'
#' @importFrom dplyr compute
compute.cdm_table <- function(x, name = attr(x, "tbl_name"), ...) {
  attr(x, "tbl_name") <- name
  return(x)
}

#' Compute a cdm_table_from_con
#'
#' @param x Table to compute.
#' @param name Name of the table.
#' @param temporary Whether to create a temporary of permanent table.
#'
#' @return The cdm_table computed
#'
#' @importFrom dplyr compute
compute.cdm_table_from_con <- function(x,
                                       name = attr(x, "tbl_name"),
                                       temporary = TRUE) {
  cdm <- attr(x, "cdm_reference")
  if (temporary) {
    name <- CDMConnector::uniqueTableName()
  }
  x <- x %>%
    CDMConnector::computeQuery(
      name = name,
      temporary = temporary,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  attr(x, "tbl_name") <- name
  return(x)
}

#' Compute a generated_cohort_set_from_con
#'
#' @param x Table to compute.
#' @param name Name of the table.
#' @param ... For compatibility.
#'
#' @return The cdm_table computed
#'
#' @importFrom dplyr compute
compute.generated_cohort_set_from_con <- function(x,
                                                  name = attr(x, "tbl_name"),
                                                  ...) {
  cdm <- attr(x, "cdm_reference")
  if (name != attr(x, "tbl_name")) {
    attr(x, "cohort_set") <- attr(x, "cohort_set") |>
      CDMConnector::computeQuery(
        name = paste0(name, "_set"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
    attr(x, "cohort_attrition") <- attr(x, "cohort_attrition") |>
      CDMConnector::computeQuery(
        name = paste0(name, "_attrition"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }
  x <- x %>%
    CDMConnector::computeQuery(
      name = name,
      temporary = temporary,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  attr(x, "tbl_name") <- name
  return(x)
}

