#' Get the cdm source of an object.
#'
#' @param x Object to get the cdm source from.
#'
#' @export
#'
#' @return The cdm source of the object.
#'
getCdmSource <- function(x) {
  UseMethod("getCdmSource")
}

#' @export
getCdmSource.tbl_df <- function(x) {
  NULL
}

#' @export
getCdmSource.cdm_reference <- function(x) {
  attr(x, "cdm_source")
}

#' @export
getCdmSource.cdm_table <- function(x) {
  getCdmSource(attr(x, "cdm_reference"))
}
