#' Clamp values to an interval
#'
#' Restricts values to the closed interval defined by `lower` and `upper`.
#'
#' @param x A numeric object.
#' @param lower,upper Numeric lower and upper bounds. Each must have size one
#'   or a size compatible with `x`.
#'
#' @return A numeric object with the shape of the longest argument.
#'
#' @details Missing values in `x` are preserved. Bounds may be infinite, but
#'   must not be missing, and `lower` must not exceed `upper`.
#'
#' @examples
#' clamp(c(-1, 0.5, 2), 0, 1)
#'
#' @export
clamp <- function(x, lower, upper) {
  .check_bounds(lower, upper)
  common <- .common_numeric(
    x,
    lower,
    upper,
    .args = c("x", "lower", "upper")
  )
  x <- common$values[[1L]]
  lower <- common$values[[2L]]
  upper <- common$values[[3L]]
  out <- pmin(pmax(x, lower), upper)
  .restore_shape(out, common$template)
}

#' Clamp values to the unit interval
#'
#' Restricts values to the closed interval from zero to one.
#'
#' @param x A numeric object.
#'
#' @return A numeric object with the same shape as `x`.
#'
#' @details Missing values are preserved.
#'
#' @examples
#' clamp01(c(-0.2, 0.4, 1.3))
#'
#' @export
clamp01 <- function(x) {
  clamp(x, 0, 1)
}

#' Test whether values are in an interval
#'
#' Tests whether values fall in the closed interval defined by `lower` and
#' `upper`.
#'
#' @param x A numeric object.
#' @param lower,upper Numeric lower and upper bounds. Each must have size one
#'   or a size compatible with `x`.
#'
#' @return A logical object with the shape of the longest argument.
#'
#' @details Missing values in `x` produce missing results. Bounds may be
#'   infinite, but must not be missing.
#'
#' @examples
#' in_range(1:5, 2, 4)
#'
#' @export
in_range <- function(x, lower, upper) {
  .check_bounds(lower, upper)
  common <- .common_numeric(
    x,
    lower,
    upper,
    .args = c("x", "lower", "upper")
  )
  x <- common$values[[1L]]
  lower <- common$values[[2L]]
  upper <- common$values[[3L]]
  out <- x >= lower & x <= upper
  .restore_shape(out, common$template)
}

#' Wrap values to a periodic interval
#'
#' Wraps values to the half-open interval from `lower` to `upper`.
#'
#' @param x A numeric object.
#' @param lower,upper Finite numeric bounds. Each must have size one or a size
#'   compatible with `x`.
#'
#' @return A numeric object with the shape of the longest argument.
#'
#' @details `lower` must be less than `upper`. Values equal to `upper` wrap to
#'   `lower`. Infinite values in `x` produce `NaN`.
#'
#' @examples
#' wrap(c(-10, 0, 370), 0, 360)
#'
#' @export
wrap <- function(x, lower, upper) {
  .check_bounds(lower, upper, finite = TRUE, strict = TRUE)
  common <- .common_numeric(
    x,
    lower,
    upper,
    .args = c("x", "lower", "upper")
  )
  x <- common$values[[1L]]
  lower <- common$values[[2L]]
  upper <- common$values[[3L]]
  width <- upper - lower
  if (any(!is.finite(width))) {
    stop("The interval width must be finite.", call. = FALSE)
  }

  offset <- (x %% width) - (lower %% width)
  out <- lower + offset %% width
  .restore_shape(out, common$template)
}
