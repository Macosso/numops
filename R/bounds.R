#' Clamp values to an interval
#'
#' Replaces values below `lower` with `lower` and values above `upper` with
#' `upper`.
#'
#' @param x A numeric vector, matrix, or array containing values to restrict.
#' @param lower A numeric lower bound giving the smallest permitted value.
#' @param upper A numeric upper bound giving the greatest permitted value.
#'
#' @return A numeric vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details Each result is computed as
#'   `min(max(x, lower), upper)`. Missing values in `x` are preserved. Bounds
#'   may be infinite, but must not be missing or have `lower > upper`.
#'
#' @section Recycling:
#' `x`, `lower`, and `upper` must each have length one or a shared length.
#' Length-one inputs are recycled; other length combinations are errors.
#' Names, dimensions, and dimnames come from the first input with the shared
#' length.
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
#' Replaces values below zero with zero and values above one with one.
#'
#' @param x A numeric vector, matrix, or array containing values to restrict.
#'
#' @return A numeric vector, matrix, or array with the same length, names,
#'   dimensions, and dimnames as `x`.
#'
#' @details This is equivalent to `clamp(x, 0, 1)`, or element-wise to
#'   `min(max(x, 0), 1)`. Missing values are preserved.
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
#' Tests whether each value falls in the closed interval from `lower` to
#' `upper`.
#'
#' @param x A numeric vector, matrix, or array containing values to test.
#' @param lower A numeric inclusive lower bound.
#' @param upper A numeric inclusive upper bound.
#'
#' @return A logical vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details Each result is computed as `x >= lower & x <= upper`. Missing
#'   values in `x` produce missing results. Bounds may be infinite, but must
#'   not be missing or have `lower > upper`.
#'
#' @section Recycling:
#' `x`, `lower`, and `upper` must each have length one or a shared length.
#' Length-one inputs are recycled; other length combinations are errors.
#' Names, dimensions, and dimnames come from the first input with the shared
#' length.
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
#' Periodically maps values to the half-open interval `[lower, upper)`.
#'
#' @param x A numeric vector, matrix, or array containing values to wrap.
#' @param lower A finite numeric lower boundary included in the result.
#' @param upper A finite numeric upper boundary excluded from the result.
#'
#' @return A numeric vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details The operation is equivalent to
#'   `lower + (x - lower) %% (upper - lower)`, using an overflow-resistant
#'   calculation. Bounds must be finite with `lower < upper`. A value equal to
#'   `upper` maps to `lower`; infinite values in `x` produce `NaN`.
#'
#' @section Recycling:
#' `x`, `lower`, and `upper` must each have length one or a shared length.
#' Length-one inputs are recycled; other length combinations are errors.
#' Names, dimensions, and dimnames come from the first input with the shared
#' length.
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
