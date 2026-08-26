#' Divide with a fallback value
#'
#' Divides corresponding values and substitutes a chosen result wherever the
#' denominator is zero.
#'
#' @param x A numeric numerator.
#' @param y A numeric denominator.
#' @param default The numeric value to return where `y == 0`. The default is
#'   `NA_real_`.
#'
#' @return A numeric vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details The result is `x / y` where `y != 0`, and `default` where
#'   `y == 0`. Missing denominators and other non-finite results follow
#'   ordinary R division; they do not trigger `default`.
#'
#' @section Recycling:
#' `x`, `y`, and `default` must each have length one or a shared length.
#' Length-one inputs are recycled; other length combinations are errors.
#' Names, dimensions, and dimnames come from the first input with the shared
#' length.
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
