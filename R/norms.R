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
#' Computes the Euclidean or L2 norm of a numeric object.
#'
#' @param x A numeric object.
#' @param margin Dimensions to retain when computing norms, or `NULL` to use
#'   all elements.
#'
#' @return A numeric scalar when `margin` is `NULL`; otherwise, a numeric
#'   object indexed by the retained dimensions.
#'
#' @details The calculation is scaled to avoid unnecessary overflow and
#'   underflow. A missing value produces a missing norm for its slice.
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
#' Scales a numeric object, or slices of it, to have Euclidean length one.
#'
#' @param x A numeric object.
#' @param margin Dimensions to retain when normalizing slices, or `NULL` to
#'   normalize all elements together.
#' @param zero How to handle zero-length slices: keep them unchanged, replace
#'   them with missing values, or throw an error.
#'
#' @return A numeric object with the same dimensions and dimnames as `x`.
#'
#' @details Missing values produce missing normalized slices. Infinite values
#'   follow ordinary division by an infinite norm.
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
