#' To add a new class that will go to the beginning.
#'
#' @export
#'
#' @return Same object with the new class added.
#'
addClass <- function(x, class) {
  x <- removeClass(x, class)
  base::class(x) <- c(class, base::class(x))
  return(x)
}

#' Remove class.
#'
#' @export
#'
#' @return Same object without the removed class.
#'
removeClass <- function(x, class) {
  base::class(x) <- base::class(x)[!(base::class(x) %in% class)]
  return(x)
}
