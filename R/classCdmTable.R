# CDM TABLE
cdmTable <- function(cdm, x, name) {
  UseMethod("cdmTable")
}

#' @export
compute.cdm_table <- function(x, name = as.character(NA)) {
  computeCdmTable(x = x, name = name)
}

