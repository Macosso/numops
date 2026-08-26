.check_numeric <- function(x, arg) {
  if (!is.numeric(x) || is.complex(x) || is.object(x)) {
    stop("`", arg, "` must be an unclassed numeric object.",
         call. = FALSE)
  }
}

.common_numeric <- function(..., .args) {
  values <- list(...)

  for (i in seq_along(values)) {
    .check_numeric(values[[i]], .args[[i]])
  }

  sizes <- lengths(values)
  size <- if (any(sizes == 0L)) 0L else max(sizes)
  valid_sizes <- if (size == 0L) c(0L, 1L) else c(1L, size)

  if (any(!sizes %in% valid_sizes)) {
    stop(
      "Arguments must have size 1 or a common size.",
      call. = FALSE
    )
  }

  template_index <- which(sizes == size)[1L]
  template <- values[[template_index]]
  values <- lapply(values, rep_len, length.out = size)

  list(values = values, template = template)
}

.restore_shape <- function(x, template) {
  template_dim <- dim(template)

  if (is.null(template_dim)) {
    names(x) <- names(template)
  } else {
    dim(x) <- template_dim
    dimnames(x) <- dimnames(template)
  }

  x
}

.check_bounds <- function(lower, upper, finite = FALSE,
                          strict = FALSE) {
  bounds <- .common_numeric(
    lower,
    upper,
    .args = c("lower", "upper")
  )$values
  lower <- bounds[[1L]]
  upper <- bounds[[2L]]

  if (anyNA(lower) || anyNA(upper)) {
    stop("`lower` and `upper` must not contain missing values.",
         call. = FALSE)
  }
  if (finite && (any(!is.finite(lower)) || any(!is.finite(upper)))) {
    stop("`lower` and `upper` must be finite.", call. = FALSE)
  }

  invalid <- if (strict) lower >= upper else lower > upper
  if (any(invalid)) {
    relation <- if (strict) "less than" else "less than or equal to"
    stop("`lower` must be ", relation, " `upper`.", call. = FALSE)
  }

  invisible(NULL)
}

.check_margin <- function(x, margin) {
  if (is.null(margin)) {
    return(NULL)
  }
  if (is.null(dim(x))) {
    stop("`margin` requires `x` to have dimensions.", call. = FALSE)
  }
  if (!is.numeric(margin) || is.complex(margin) || anyNA(margin) ||
      any(!is.finite(margin)) || any(margin != floor(margin))) {
    stop("`margin` must contain whole numbers.", call. = FALSE)
  }

  if (!length(margin) || anyDuplicated(margin) ||
      any(margin < 1 | margin > length(dim(x)))) {
    stop("`margin` must select unique dimensions of `x`.",
         call. = FALSE)
  }

  as.integer(margin)
}
