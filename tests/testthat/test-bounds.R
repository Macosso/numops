test_that("bounds operations return expected values", {
  expect_identical(
    clamp(c(-1, 0.5, 2), 0, 1),
    c(0, 0.5, 1)
  )
  expect_identical(
    clamp01(c(-1, NA, 2)),
    c(0, NA_real_, 1)
  )
  expect_identical(
    in_range(1:5, 2, 4),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    wrap(c(-10, 0, 360, 370), 0, 360),
    c(350, 0, 0, 10)
  )
})

test_that("bounds operations preserve shape", {
  x <- matrix(
    -2:3,
    nrow = 2,
    dimnames = list(c("a", "b"), NULL)
  )
  result <- clamp(x, 0, 2)

  expect_identical(dim(result), dim(x))
  expect_identical(dimnames(result), dimnames(x))
})

test_that("wrap avoids unnecessary overflow", {
  result <- wrap(.Machine$double.xmax, -1e308, -5e307)

  expect_true(is.finite(result))
  expect_true(in_range(result, -1e308, -5e307))
})

test_that("bounds operations validate inputs", {
  expect_error(clamp(1, 2, 1), "less than or equal")
  expect_error(clamp(numeric(), NA_real_, 1), "missing")
  expect_error(wrap(1, 0, 0), "less than")
  expect_error(
    wrap(1, -.Machine$double.xmax, .Machine$double.xmax),
    "width"
  )
  expect_error(clamp(1:3, 1:2, 4), "common size")
})
