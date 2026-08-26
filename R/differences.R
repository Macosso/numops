#' Adjacent differences with preserved length
#'
#' Keeps the first value and replaces every later value with its change from
#' the preceding value.
#'
#' @param x A numeric vector in the order in which differences are required.
#'
#' @return A numeric vector with the same length and names as `x`.
#'
#' @details For nonempty `x`, the result satisfies `result[1] = x[1]` and
#'   `result[i] = x[i] - x[i - 1]` for later positions. Unlike [diff()], the
#'   first value is retained, so the input can be recovered with [cumsum()].
#'   An empty input is returned unchanged.
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
