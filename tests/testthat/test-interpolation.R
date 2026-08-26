test_that("interpolation operations return expected values", {
  expect_identical(
    lerp(10, 20, c(0, 0.5, 1, 2)),
    c(10, 15, 20, 30)
  )
  expect_identical(
    inv_lerp(10, 20, c(10, 15, 20)),
    c(0, 0.5, 1)
  )
  expect_identical(
    remap(c(0, 5, 10), c(0, 10), c(-1, 1)),
    c(-1, 0, 1)
  )
  expect_identical(
    remap(c(10, 0), c(10, 0), c(0, 1)),
    c(0, 1)
  )
  expect_true(is.na(lerp(0, 1, NA_real_)))
  expect_true(is.na(inv_lerp(NA_real_, 1, 0)))
})

test_that("interpolation avoids unnecessary overflow", {
  largest <- .Machine$double.xmax

  expect_identical(lerp(-largest, largest, 0.5), 0)
  expect_identical(midpoint(-largest, largest), 0)
  expect_identical(inv_lerp(-largest, largest, 0), 0.5)
})

test_that("interpolation operations validate intervals", {
  expect_error(inv_lerp(1, 1, numeric()), "must differ")
  expect_error(inv_lerp(-Inf, 1, 0), "must not be infinite")
  expect_error(remap(1, c(0, 0), c(0, 1)), "must differ")
  expect_error(remap(1, c(0, 1, 2), c(0, 1)), "two finite")
})

test_that("divide_or substitutes only zero denominators", {
  expect_identical(
    divide_or(c(4, 5, 0), c(2, 0, 0), -1),
    c(2, -1, -1)
  )
  expect_true(is.na(divide_or(1, NA_real_)))
  expect_identical(divide_or(1:3, 1), as.numeric(1:3))
  expect_error(divide_or(1:3, 1:2), "common size")
})
