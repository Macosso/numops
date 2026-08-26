.l2_norm_vector <- function(x) {
  if (anyNA(x)) {
    return(NA_real_)
  }
  if (any(is.infinite(x))) {
    return(Inf)
  }
  if (!length(x)) {
    return(0)
  }

  scale <- max(abs(x))
  if (scale == 0) {
    return(0)
  }

  scale * sqrt(sum((x / scale)^2))
}

#' Euclidean norm
#'
#' Computes Euclidean lengths for a complete object or for slices selected by
#' `margin`.
#'
#' @param x A numeric vector, matrix, or array.
#' @param margin An integer vector naming the dimensions that index separate
#'   slices, or `NULL` to treat all elements as one vector. For a matrix,
#'   `margin = 1` computes row norms and `margin = 2` computes column norms.
#'
#' @return If `margin` is `NULL`, one numeric value. Otherwise, a numeric vector
#'   or array indexed by `dim(x)[margin]`, with the corresponding dimnames.
#'
#' @details For a slice with values `x[i]`, the L2 norm is
#'   `sqrt(sum(x[i]^2))`. The calculation is scaled to avoid unnecessary
#'   overflow and underflow. An empty slice has norm zero, an infinite value
#'   produces an infinite norm, and a missing value produces a missing norm.
#'
#' @examples
#' l2_norm(c(3, 4))
#' l2_norm(matrix(1:6, nrow = 2), margin = 1)
#'
#' @export
l2_norm <- function(x, margin = NULL) {
  .check_numeric(x, "x")
  margin <- .check_margin(x, margin)

  if (is.null(margin)) {
    return(.l2_norm_vector(x))
  }

  apply(x, margin, .l2_norm_vector)
}

#' Normalize to unit Euclidean length
#'
#' Divides a numeric object, or each selected slice, by its Euclidean norm.
#'
#' @param x A numeric vector, matrix, or array to normalize.
#' @param margin An integer vector naming the dimensions that index separate
#'   slices, or `NULL` to normalize all elements together. For a matrix,
#'   `margin = 1` normalizes rows and `margin = 2` normalizes columns.
#' @param zero How to handle a slice whose norm is zero. `"keep"` leaves the
#'   slice unchanged, `"na"` replaces it with missing values, and `"error"`
#'   stops the calculation.
#'
#' @return A numeric vector, matrix, or array with the same length, names,
#'   dimensions, and dimnames as `x`.
#'
#' @details Each slice `s` is transformed to `s / l2_norm(s)`. Nonzero finite
#'   slices therefore have an L2 norm of one. A missing value makes its entire
#'   slice missing. Infinite values follow ordinary division by an infinite
#'   norm.
#'
#' @examples
#' normalize_l2(c(3, 4))
#' normalize_l2(matrix(1:6, nrow = 2), margin = 1)
#'
#' @export
normalize_l2 <- function(x, margin = NULL,
                         zero = c("keep", "na", "error")) {
  .check_numeric(x, "x")
  margin <- .check_margin(x, margin)
  zero <- match.arg(zero)
  norms <- l2_norm(x, margin)

  if (zero == "error" && any(norms == 0, na.rm = TRUE)) {
    stop("Cannot normalize a zero-length slice.", call. = FALSE)
  }
  if (zero == "keep") {
    norms[norms == 0 & !is.na(norms)] <- 1
  } else if (zero == "na") {
    norms[norms == 0 & !is.na(norms)] <- NA_real_
  }

  if (is.null(margin)) {
    return(x / norms)
  }

  sweep(x, margin, norms, "/", check.margin = TRUE)
}
