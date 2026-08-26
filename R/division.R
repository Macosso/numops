#' Divide with a fallback value
#'
#' Divides `x` by `y` and substitutes `default` where `y` is zero.
#'
#' @param x,y Numeric objects.
#' @param default A numeric fallback value.
#'
#' @return A numeric object with the shape of the longest argument.
#'
#' @details Only zero denominators trigger the fallback. Other missing or
#'   non-finite results follow ordinary R division. Arguments use strict
#'   scalar recycling.
#'
#' @examples
#' divide_or(c(1, 2, 0), c(1, 0, 0), default = NA_real_)
#'
#' @export
divide_or <- function(x, y, default = NA_real_) {
  common <- .common_numeric(
    x,
    y,
    default,
    .args = c("x", "y", "default")
  )
  x <- common$values[[1L]]
  y <- common$values[[2L]]
  default <- common$values[[3L]]

  out <- x / y
  use_default <- y == 0 & !is.na(y)
  out[use_default] <- default[use_default]

  .restore_shape(out, common$template)
}
