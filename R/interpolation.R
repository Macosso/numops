#' Linear interpolation
#'
#' Computes the value a proportion `t` of the way from `a` to `b`.
#'
#' @param a A numeric endpoint returned when `t = 0`.
#' @param b A numeric endpoint returned when `t = 1`.
#' @param t A numeric interpolation proportion, usually between zero and one.
#'
#' @return A numeric vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details Each result is computed as `a + t * (b - a)`, using a calculation
#'   that avoids unnecessary overflow when `a` and `b` have opposite signs.
#'   Values of `t` outside `[0, 1]` extrapolate. The endpoints are returned
#'   exactly when `t` is zero or one.
#'
#' @section Recycling:
#' `a`, `b`, and `t` must each have length one or a shared length. Length-one
#' inputs are recycled; other length combinations are errors. Names,
#' dimensions, and dimnames come from the first input with the shared length.
#'
#' @examples
#' lerp(10, 20, c(0, 0.25, 1))
#'
#' @export
lerp <- function(a, b, t) {
  common <- .common_numeric(
    a,
    b,
    t,
    .args = c("a", "b", "t")
  )
  a <- common$values[[1L]]
  b <- common$values[[2L]]
  t <- common$values[[3L]]

  opposite_signs <- (a < 0 & b > 0) | (a > 0 & b < 0)
  opposite_signs[is.na(opposite_signs)] <- FALSE
  out <- a + t * (b - a)
  out[opposite_signs] <- (
    (1 - t[opposite_signs]) * a[opposite_signs] +
      t[opposite_signs] * b[opposite_signs]
  )
  at_start <- t == 0 & !is.na(t)
  at_end <- t == 1 & !is.na(t)
  out[at_start] <- a[at_start]
  out[at_end] <- b[at_end]

  .restore_shape(out, common$template)
}

#' Inverse linear interpolation
#'
#' Calculates how far `x` lies from `a` toward `b`.
#'
#' @param a A numeric endpoint corresponding to a result of zero.
#' @param b A numeric endpoint corresponding to a result of one. It must differ
#'   from `a` at every non-missing position.
#' @param x A numeric vector, matrix, or array containing values to locate.
#'
#' @return A numeric vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details Each result is `(x - a) / (b - a)`, calculated to avoid
#'   unnecessary overflow for widely separated endpoints. Results outside
#'   `[0, 1]` indicate that `x` lies outside the endpoints. Infinite endpoints
#'   are not supported, and missing values are propagated.
#'
#' @section Recycling:
#' `a`, `b`, and `x` must each have length one or a shared length. Length-one
#' inputs are recycled; other length combinations are errors. Names,
#' dimensions, and dimnames come from the first input with the shared length.
#'
#' @examples
#' inv_lerp(10, 20, c(10, 15, 20))
#'
#' @export
inv_lerp <- function(a, b, x) {
  endpoints <- .common_numeric(a, b, .args = c("a", "b"))$values
  if (any(is.infinite(endpoints[[1L]])) ||
      any(is.infinite(endpoints[[2L]]))) {
    stop("`a` and `b` must not be infinite.", call. = FALSE)
  }
  if (any(endpoints[[1L]] == endpoints[[2L]], na.rm = TRUE)) {
    stop("`a` and `b` must differ.", call. = FALSE)
  }

  common <- .common_numeric(
    a,
    b,
    x,
    .args = c("a", "b", "x")
  )
  a <- common$values[[1L]]
  b <- common$values[[2L]]
  x <- common$values[[3L]]

  denominator <- b - a
  out <- (x - a) / denominator
  unstable <- !is.finite(denominator) | denominator == 0
  unstable[is.na(unstable)] <- FALSE

  if (any(unstable)) {
    scale <- pmax(abs(a[unstable]), abs(b[unstable]))
    out[unstable] <- (
      (x[unstable] / scale - a[unstable] / scale) /
        (b[unstable] / scale - a[unstable] / scale)
    )
  }

  .restore_shape(out, common$template)
}

#' Remap values between intervals
#'
#' Linearly maps values from the interval `from` to the interval `to`.
#'
#' @param x A numeric vector, matrix, or array containing values to map.
#' @param from A finite numeric vector of length two giving the input endpoints.
#' @param to A finite numeric vector of length two giving the output endpoints.
#'
#' @return A numeric vector, matrix, or array with the same length, names,
#'   dimensions, and dimnames as `x`.
#'
#' @details The result is
#'   `to[1] + (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1])`.
#'   Values outside `from` are extrapolated. Either interval may be reversed,
#'   but the endpoints of `from` must differ.
#'
#' @examples
#' remap(c(0, 5, 10), c(0, 10), c(-1, 1))
#'
#' @export
remap <- function(x, from, to) {
  .check_numeric(x, "x")
  .check_numeric(from, "from")
  .check_numeric(to, "to")

  if (length(from) != 2L || any(!is.finite(from))) {
    stop("`from` must contain two finite values.", call. = FALSE)
  }
  if (length(to) != 2L || any(!is.finite(to))) {
    stop("`to` must contain two finite values.", call. = FALSE)
  }
  if (from[[1L]] == from[[2L]]) {
    stop("The endpoints of `from` must differ.", call. = FALSE)
  }

  position <- inv_lerp(from[[1L]], from[[2L]], x)
  out <- lerp(to[[1L]], to[[2L]], position)
  .restore_shape(out, x)
}

#' Midpoint between values
#'
#' Computes the value halfway between corresponding values in `x` and `y`.
#'
#' @param x The first numeric endpoint.
#' @param y The second numeric endpoint.
#'
#' @return A numeric vector, matrix, or array of the shared length. It takes
#'   names, dimensions, and dimnames from the first input already having that
#'   length.
#'
#' @details The mathematical result is `(x + y) / 2`. The implementation uses
#'   equivalent forms chosen to avoid unnecessary overflow for finite values.
#'   Missing and infinite values follow ordinary R arithmetic.
#'
#' @section Recycling:
#' `x` and `y` must each have length one or a shared length. Length-one inputs
#' are recycled; other length combinations are errors. Names, dimensions, and
#' dimnames come from the first input with the shared length.
#'
#' @examples
#' midpoint(c(0, 10), c(10, 20))
#'
#' @export
midpoint <- function(x, y) {
  common <- .common_numeric(x, y, .args = c("x", "y"))
  x <- common$values[[1L]]
  y <- common$values[[2L]]

  opposite_signs <- (x < 0 & y > 0) | (x > 0 & y < 0)
  opposite_signs[is.na(opposite_signs)] <- FALSE
  out <- x + (y - x) / 2
  out[opposite_signs] <- (x[opposite_signs] + y[opposite_signs]) / 2

  .restore_shape(out, common$template)
}
