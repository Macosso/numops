library(numops)

expect_error <- function(code, pattern = NULL) {
  error <- tryCatch(
    {
      force(code)
      NULL
    },
    error = identity
  )

  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) {
    stopifnot(grepl(pattern, conditionMessage(error), fixed = TRUE))
  }
}

# Bounds -------------------------------------------------------------------

stopifnot(
  identical(clamp(c(-1, 0.5, 2), 0, 1), c(0, 0.5, 1)),
  identical(clamp01(c(-1, NA, 2)), c(0, NA_real_, 1)),
  identical(in_range(1:5, 2, 4), c(FALSE, TRUE, TRUE, TRUE, FALSE)),
  identical(wrap(c(-10, 0, 360, 370), 0, 360), c(350, 0, 0, 10))
)

large_wrapped <- wrap(.Machine$double.xmax, -1e308, -5e307)
stopifnot(is.finite(large_wrapped), in_range(large_wrapped, -1e308, -5e307))

x_matrix <- matrix(-2:3, nrow = 2, dimnames = list(c("a", "b"), NULL))
clamped_matrix <- clamp(x_matrix, 0, 2)
stopifnot(
  identical(dim(clamped_matrix), dim(x_matrix)),
  identical(dimnames(clamped_matrix), dimnames(x_matrix))
)

expect_error(clamp(1, 2, 1), "less than or equal")
expect_error(clamp(numeric(), NA_real_, 1), "missing")
expect_error(wrap(1, 0, 0), "less than")
expect_error(wrap(1, -.Machine$double.xmax, .Machine$double.xmax),
             "width")
expect_error(clamp(1:3, 1:2, 4), "common size")

# Interpolation ------------------------------------------------------------

stopifnot(
  identical(lerp(10, 20, c(0, 0.5, 1, 2)), c(10, 15, 20, 30)),
  identical(inv_lerp(10, 20, c(10, 15, 20)), c(0, 0.5, 1)),
  identical(remap(c(0, 5, 10), c(0, 10), c(-1, 1)), c(-1, 0, 1)),
  identical(remap(c(10, 0), c(10, 0), c(0, 1)), c(0, 1)),
  is.na(lerp(0, 1, NA_real_)),
  is.na(inv_lerp(NA_real_, 1, 0))
)

largest <- .Machine$double.xmax
stopifnot(
  identical(lerp(-largest, largest, 0.5), 0),
  identical(midpoint(-largest, largest), 0),
  identical(inv_lerp(-largest, largest, 0), 0.5)
)

expect_error(inv_lerp(1, 1, numeric()), "must differ")
expect_error(inv_lerp(-Inf, 1, 0), "must not be infinite")
expect_error(remap(1, c(0, 0), c(0, 1)), "must differ")
expect_error(remap(1, c(0, 1, 2), c(0, 1)), "two finite")

# Division -----------------------------------------------------------------

stopifnot(
  identical(divide_or(c(4, 5, 0), c(2, 0, 0), -1), c(2, -1, -1)),
  is.na(divide_or(1, NA_real_)),
  identical(divide_or(1:3, 1), as.numeric(1:3))
)

expect_error(divide_or(1:3, 1:2), "common size")

# Norms --------------------------------------------------------------------

stopifnot(
  identical(l2_norm(c(3, 4)), 5),
  identical(l2_norm(numeric()), 0),
  is.na(l2_norm(c(1, NA_real_))),
  is.infinite(l2_norm(c(1, Inf)))
)

large_unit <- normalize_l2(c(1e308, 1e308))
stopifnot(isTRUE(all.equal(large_unit, rep(sqrt(0.5), 2))))

small_norm <- l2_norm(c(1e-300, 1e-300))
stopifnot(small_norm > 0, is.finite(small_norm))

norm_matrix <- matrix(c(3, 4, 0, 0), nrow = 2, byrow = TRUE)
rownames(norm_matrix) <- c("nonzero", "zero")
stopifnot(
  identical(unname(l2_norm(norm_matrix, 1)), c(5, 0)),
  identical(normalize_l2(norm_matrix, 1)[2, ], c(0, 0)),
  all(is.na(normalize_l2(norm_matrix, 1, zero = "na")[2, ])),
  identical(rownames(normalize_l2(norm_matrix, 1)), rownames(norm_matrix))
)

normalized_array <- normalize_l2(array(1:8, c(2, 2, 2)), margin = c(1, 3))
stopifnot(
  identical(dim(normalized_array), c(2L, 2L, 2L)),
  isTRUE(all.equal(as.vector(l2_norm(normalized_array, c(1, 3))),
                   rep(1, 4)))
)

expect_error(normalize_l2(c(0, 0), zero = "error"), "zero-length")
expect_error(l2_norm(1:3, margin = 1), "requires")
expect_error(l2_norm(matrix(1:4, 2), margin = c(1, 1)), "unique")

# Differences and validation ----------------------------------------------

named_values <- c(a = 10, b = 13, c = 12)
stopifnot(
  identical(adjacent_difference(named_values), c(a = 10, b = 3, c = -1)),
  identical(adjacent_difference(numeric()), numeric())
)

expect_error(adjacent_difference(matrix(1:4, 2)), "without dimensions")
expect_error(clamp(factor("a"), 0, 1), "unclassed numeric")
expect_error(midpoint(1 + 1i, 2), "unclassed numeric")
