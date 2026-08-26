#' Linear interpolation
#'
#' Interpolates from `a` to `b` by the proportion `t`.
#'
#' @param a,b Numeric endpoints.
#' @param t A numeric interpolation proportion.
#'
#' @return A numeric object with the shape of the longest argument.
#'
#' @details Values of `t` outside zero and one extrapolate. The endpoints are
#'   returned exactly when `t` is zero or one.
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
#' Calculates the relative position of `x` between `a` and `b`.
#'
#' @param a,b Numeric endpoints. They must differ at every position.
#' @param x A numeric object containing values to locate.
#'
#' @return A numeric object with the shape of the longest argument.
#'
#' @details Results may be outside zero and one. Infinite endpoints are not
#'   supported. Missing values are propagated.
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
#' Maps values from one interval to another by linear interpolation.
#'
#' @param x A numeric object.
#' @param from A finite numeric vector of length two defining the input
#'   interval.
#' @param to A finite numeric vector of length two defining the output
#'   interval.
#'
#' @return A numeric object with the same shape as `x`.
#'
#' @details Values outside `from` are extrapolated. The input interval may be
#'   reversed, but its endpoints must differ.
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
#' Computes the midpoint between corresponding values in `x` and `y`.
#'
#' @param x,y Numeric objects.
#'
#' @return A numeric object with the shape of the longest argument.
#'
#' @details The calculation avoids avoidable overflow for finite values.
#'   Missing and infinite values follow ordinary R arithmetic.
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
