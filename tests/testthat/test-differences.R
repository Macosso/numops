test_that("adjacent_difference preserves length and names", {
  x <- c(a = 10, b = 13, c = 12)

  expect_identical(
    adjacent_difference(x),
    c(a = 10, b = 3, c = -1)
  )
  expect_identical(adjacent_difference(numeric()), numeric())
})

test_that("numeric inputs are validated", {
  expect_error(
    adjacent_difference(matrix(1:4, 2)),
    "without dimensions"
  )
  expect_error(clamp(factor("a"), 0, 1), "unclassed numeric")
  expect_error(midpoint(1 + 1i, 2), "unclassed numeric")
})
