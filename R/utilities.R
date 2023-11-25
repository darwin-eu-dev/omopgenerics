`addClass<-` <- function(x, value) {
  removeClass(x) <- value
  base::class(x) <- c(value, base::class(x))
  return(x)
}
`removeClass<-` <- function(x, value) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  return(x)
}
