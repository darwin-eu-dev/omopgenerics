`addClass<-` <- function(x, value) {
  removeClass(x) <- value
  base::class(x) <- c(value, base::class(x))
  return(x)
}
`removeClass<-` <- function(x, value) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
getCdmSource <- function(x) {
  if ("cdm_reference" %in% class(x)) {
    cdm <- x
  } else {
    cdm <- attr(x, "cdm_reference")
  }
  if (is.null(cdm)) {
    return(NULL)
  } else {
    return(attr(cdm, "cdm_source"))
  }
}

