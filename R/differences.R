#' Adjacent differences with preserved length
#'
#' Returns the first value followed by differences between adjacent values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with the same length and names as `x`.
#'
#' @details Unlike [diff()], the result has the same length as the input. An
#'   empty input is returned unchanged.
#'
#' @examples
#' adjacent_difference(c(10, 13, 12))
#'
#' @export
adjacent_difference <- function(x) {
  .check_numeric(x, "x")
  if (!is.null(dim(x))) {
    stop("`x` must be a vector without dimensions.", call. = FALSE)
  }
  if (!length(x)) {
    return(x)
  }

  out <- c(x[[1L]], diff(x))
  names(out) <- names(x)
  out
}
