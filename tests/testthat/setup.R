addClassAndAttributes <- function(x) {
  addClass(x) <- "cdm_table"
  attr(x, "cdm_reference") <- 1
  attr(x, "tbl_name") <- "xuxudecrema"
  return(x)
}
