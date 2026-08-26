test_that("l2_norm handles ordinary and non-finite values", {
  expect_identical(l2_norm(c(3, 4)), 5)
  expect_identical(l2_norm(numeric()), 0)
  expect_true(is.na(l2_norm(c(1, NA_real_))))
  expect_true(is.infinite(l2_norm(c(1, Inf))))
})

test_that("norm calculations avoid unnecessary range loss", {
  large_unit <- normalize_l2(c(1e308, 1e308))
  expect_equal(large_unit, rep(sqrt(0.5), 2))

  small_norm <- l2_norm(c(1e-300, 1e-300))
  expect_gt(small_norm, 0)
  expect_true(is.finite(small_norm))
})

test_that("margin normalization handles zero slices", {
  x <- matrix(c(3, 4, 0, 0), nrow = 2, byrow = TRUE)
  rownames(x) <- c("nonzero", "zero")

  expect_identical(unname(l2_norm(x, 1)), c(5, 0))
  expect_identical(normalize_l2(x, 1)[2, ], c(0, 0))
  expect_true(all(is.na(normalize_l2(x, 1, zero = "na")[2, ])))
  expect_identical(rownames(normalize_l2(x, 1)), rownames(x))
  expect_error(normalize_l2(c(0, 0), zero = "error"), "zero-length")
})

test_that("normalization supports arrays", {
  result <- normalize_l2(array(1:8, c(2, 2, 2)), margin = c(1, 3))

  expect_identical(dim(result), c(2L, 2L, 2L))
  expect_equal(
    as.vector(l2_norm(result, c(1, 3))),
    rep(1, 4)
  )
})

test_that("margin arguments are validated", {
  expect_error(l2_norm(1:3, margin = 1), "requires")
  expect_error(l2_norm(matrix(1:4, 2), margin = c(1, 1)), "unique")
})
