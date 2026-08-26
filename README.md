
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# numops

<!-- badges: start -->

[![R-CMD-check](https://github.com/Macosso/numops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Macosso/numops/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

numops provides small, dependency-free numerical operations for vectors,
matrices, and arrays. It collects common tasks that otherwise require
repeated base R expressions and gives them consistent validation, scalar
recycling, and shape-preserving behavior.

## Installation

You can install the development version of numops from
[GitHub](https://github.com/Macosso/numops) with:

``` r
# install.packages("pak")
pak::pak("Macosso/numops")
```

## Overview

| Task | Functions |
|----|----|
| Restrict or wrap values | `clamp()`, `clamp01()`, `in_range()`, `wrap()` |
| Transform ranges | `lerp()`, `inv_lerp()`, `remap()`, `midpoint()` |
| Handle division by zero | `divide_or()` |
| Compute or normalize Euclidean length | `l2_norm()`, `normalize_l2()` |
| Preserve-length differences | `adjacent_difference()` |

## Usage

``` r
library(numops)
```

### Bounds and periodic values

Clamp values to a closed interval or wrap them to a half-open periodic
interval:

``` r
clamp01(c(-1, 0.25, 1.5))
#> [1] 0.00 0.25 1.00
wrap(c(-10, 0, 370), lower = 0, upper = 360)
#> [1] 350   0  10
```

### Interpolation and remapping

Interpolate between endpoints or map values from one interval to
another:

``` r
lerp(10, 20, c(0, 0.25, 1))
#> [1] 10.0 12.5 20.0
remap(c(0, 50, 100), from = c(0, 100), to = c(-1, 1))
#> [1] -1  0  1
```

Values outside the source interval are extrapolated rather than clamped.

### Division with a fallback

Choose the result returned where a denominator is zero:

``` r
divide_or(
  c(12, 8, 5),
  c(3, 0, 2),
  default = NA_real_
)
#> [1] 4.0  NA 2.5
```

### Vector, matrix, and array norms

Use `margin` to compute or normalize independent slices. For matrices,
`margin = 1` selects rows and `margin = 2` selects columns:

``` r
x <- matrix(
  c(3, 4, 0, 1, 2, 2),
  nrow = 2,
  byrow = TRUE
)

l2_norm(x, margin = 1)
#> [1] 5 3
normalize_l2(x, margin = 1)
#>           [,1]      [,2]      [,3]
#> [1,] 0.6000000 0.8000000 0.0000000
#> [2,] 0.3333333 0.6666667 0.6666667
```

## Recycling and output shape

Functions with multiple numeric inputs use strict scalar recycling. Each
input must have length one or a shared length; other length combinations
are errors. Length-one inputs are recycled to the shared length.

Names, dimensions, and dimnames are copied from the first input already
having the shared length. Functions that transform a single object
preserve its shape.

## Getting help

If you find a bug or have a feature request, please open an issue on the
[GitHub issue tracker](https://github.com/Macosso/numops/issues).
