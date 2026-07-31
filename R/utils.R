.safe_vector <- function(x, name) {
  if (is.ordered(x)) {
    x <- as.integer(x)
  } else if (is.factor(x)) {
    stop(
      sprintf("`%s` is an unordered factor. Supply a numeric vector or an ordered factor.", name),
      call. = FALSE
    )
  } else if (is.logical(x)) {
    x <- as.integer(x)
  } else if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric, logical, or an ordered factor.", name), call. = FALSE)
  }

  x <- as.numeric(x)
  if (length(x) == 0L) {
    stop(sprintf("`%s` must not be empty.", name), call. = FALSE)
  }
  x
}

.prepare_pair <- function(reference, ranking, na_rm) {
  reference <- .safe_vector(reference, "reference")
  ranking <- .safe_vector(ranking, "ranking")

  if (length(reference) != length(ranking)) {
    stop("`reference` and `ranking` must have the same length.", call. = FALSE)
  }

  bad <- !is.finite(reference) | !is.finite(ranking)
  if (any(bad)) {
    if (!isTRUE(na_rm)) {
      stop(
        "Missing or non-finite values found. Use `na_rm = TRUE` to remove incomplete pairs.",
        call. = FALSE
      )
    }
    reference <- reference[!bad]
    ranking <- ranking[!bad]
  }

  if (length(reference) < 2L) {
    stop("At least two complete observations are required.", call. = FALSE)
  }

  list(reference = reference, ranking = ranking)
}


.validate_round_digits <- function(round_digits) {
  if (is.null(round_digits)) {
    return(NULL)
  }
  if (length(round_digits) != 1L || !is.finite(round_digits) || round_digits < 0) {
    stop("`round_digits` must be NULL or one non-negative finite number.", call. = FALSE)
  }
  as.integer(round_digits)
}

.clamp_unit <- function(x, tolerance = 1e-12) {
  if (is.na(x)) {
    return(x)
  }
  if (x < 0 && x > -tolerance) x <- 0
  if (x > 1 && x < 1 + tolerance) x <- 1
  x
}

.complete_vector <- function(x, name, na_rm) {
  x <- .safe_vector(x, name)
  bad <- !is.finite(x)
  if (any(bad)) {
    if (!isTRUE(na_rm)) {
      stop(
        sprintf("`%s` contains missing or non-finite values. Use `na_rm = TRUE` to remove them.", name),
        call. = FALSE
      )
    }
    x <- x[!bad]
  }
  if (length(x) < 2L) {
    stop(sprintf("`%s` must contain at least two complete observations.", name), call. = FALSE)
  }
  x
}
