# packages can not depend on DBI/CDMConnector

#' @export
`[[<-.cdm_reference` <- function(obj, name, value) {
  if (!"cdm_table" %in% value) {
    value <- cdmTable(cdm = obj, x = value, name = name)
  }
  x <- class(obj)
  attr(value, "cdm_reference") <- NULL
  obj <- unclass(obj)
  obj[[name]] <- value
  class(obj) <- x
  return(obj)
}

# CDM TABLE
cdmTable <- function(cdm, x, name) {
  UseMethod("cdmTable")
}
cdmTable.cdm_reference <- function(cdm, x, name) {
  addClass(x, "cdm_table")
}
cdmTable.cdm_reference_from_con <- function(cdm, x, name) {
  if (!"tbl_df" %in% class(x)) {
    cli::cli_abort("x must be a tbl_df table.")
  }
  fullName <- CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = name)
  con <- attr(cdm, "dbcon")
  DBI::dbWriteTable(conn = con, name = fullName, value = x)
  x <- dplyr::tbl(src = con, fullName)
  attr(x, "tbl_name") <- name
  x <- addClass(x, c("cdm_table_from_con", "cdm_table"))
  return(x)
}

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

