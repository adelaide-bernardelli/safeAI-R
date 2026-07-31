#' Lorenz, Dual Lorenz, and Concordance Curves
#'
#' Construct the normalized cumulative curves underlying Rank Graduation
#' metrics.
#'
#' @details
#' `lorenz_curve()` orders the supplied values increasingly and calculates
#' their cumulative shares.
#'
#' `dual_lorenz_curve()` orders the supplied values decreasingly and calculates
#' their cumulative shares.
#'
#' `concordance_curve()` orders a reference vector according to a second
#' ranking vector and then calculates the cumulative shares of the ordered
#' reference values. Ties in the ranking vector are handled using the same
#' tie-group averaging rule employed by the Rank Graduation metrics.
#'
#' By default, each returned data frame includes the origin `(0, 0)`, making
#' the curve coordinates ready for base-R or `ggplot2` graphics. Normalization
#' requires the relevant vector to have a non-zero sum. For the usual Lorenz-
#' curve interpretation, the supplied values should also be non-negative.
#'
#' These functions return curve coordinates; they do not draw the curves.
#' Use [rg_curves()] to collect all four relevant curves in one long-format
#' data frame.
#'
#' @param x Numeric vector, logical vector, or ordered factor whose cumulative
#'   shares are calculated.
#' @param na_rm Logical. If `TRUE`, missing and non-finite values are removed.
#'   If `FALSE`, they produce an error.
#' @param include_origin Logical. If `TRUE`, include the point `(0, 0)`.
#'
#' @return
#' A data frame with columns `p`, the cumulative proportion of observations,
#' and `share`, the corresponding cumulative share of the supplied values.
#'
#' @examples
#' actual <- c(2, 3, 5, 7)
#' predicted <- c(3, 7, 2, 5)
#'
#' # Increasing benchmark
#' lorenz_curve(actual)
#'
#' # Decreasing benchmark
#' dual_lorenz_curve(actual)
#'
#' # Outcomes ordered according to the predictions
#' concordance_curve(
#'   reference = actual,
#'   ranking = predicted
#' )
#'
#' @seealso [rg_curves()] and [rga()].
#' @export
lorenz_curve <- function(x, na_rm = FALSE, include_origin = TRUE) {
  x <- .complete_vector(x, "x", na_rm = na_rm)
  total <- sum(x)
  if (!is.finite(total) || abs(total) <= .Machine$double.eps) {
    stop(
      "The curve cannot be normalized because the vector sum is zero.",
      call. = FALSE
    )
  }

  n <- length(x)
  out <- data.frame(
    p = seq_len(n) / n,
    share = cumsum(sort(x, decreasing = FALSE)) / total
  )

  if (isTRUE(include_origin)) {
    out <- rbind(data.frame(p = 0, share = 0), out)
  }

  rownames(out) <- NULL
  out
}

#' @rdname lorenz_curve
#' @export
dual_lorenz_curve <- function(x, na_rm = FALSE, include_origin = TRUE) {
  x <- .complete_vector(x, "x", na_rm = na_rm)
  total <- sum(x)
  if (!is.finite(total) || abs(total) <= .Machine$double.eps) {
    stop(
      "The curve cannot be normalized because the vector sum is zero.",
      call. = FALSE
    )
  }

  n <- length(x)
  out <- data.frame(
    p = seq_len(n) / n,
    share = cumsum(sort(x, decreasing = TRUE)) / total
  )

  if (isTRUE(include_origin)) {
    out <- rbind(data.frame(p = 0, share = 0), out)
  }

  rownames(out) <- NULL
  out
}

#' @rdname lorenz_curve
#' @param reference Numeric vector, logical vector, or ordered factor whose
#'   cumulative shares are calculated.
#' @param ranking Numeric vector, logical vector, or ordered factor that induces
#'   the ordering of `reference`.
#' @param round_digits Number of decimal places used to identify ties in
#'   `ranking`. Use `NULL` to disable rounding before tie detection.
#' @export
concordance_curve <- function(
  reference,
  ranking,
  round_digits = 6L,
  na_rm = FALSE,
  include_origin = TRUE
) {
  pair <- .prepare_pair(reference, ranking, na_rm = na_rm)
  reference <- pair$reference
  ranking <- pair$ranking
  round_digits <- .validate_round_digits(round_digits)

  total <- sum(reference)
  if (!is.finite(total) || abs(total) <= .Machine$double.eps) {
    stop(
      "The curve cannot be normalized because the reference sum is zero.",
      call. = FALSE
    )
  }

  key <- if (is.null(round_digits)) ranking else round(ranking, round_digits)
  groups <- match(key, sort(unique(key)))
  tied_reference <- stats::ave(reference, groups, FUN = mean)
  ordered_reference <- tied_reference[order(groups, seq_along(groups))]

  n <- length(reference)
  out <- data.frame(
    p = seq_len(n) / n,
    share = cumsum(ordered_reference) / total
  )

  if (isTRUE(include_origin)) {
    out <- rbind(data.frame(p = 0, share = 0), out)
  }

  rownames(out) <- NULL
  out
}

#' Collect Rank Graduation Curves
#'
#' Combine the Lorenz, dual Lorenz, concordance, and equality curves in a
#' single long-format data frame.
#'
#' @details
#' `rg_curves()` is a convenience function for plotting or comparing all four
#' curves underlying a Rank Graduation analysis. The Lorenz and dual Lorenz
#' curves provide the increasing and decreasing benchmark orderings, the
#' concordance curve reflects the user-supplied ranking, and the equality curve
#' is the 45-degree line.
#'
#' @param reference Numeric vector, logical vector, or ordered factor whose
#'   cumulative shares are calculated.
#' @param ranking Numeric vector, logical vector, or ordered factor that induces
#'   the concordance ordering.
#' @param round_digits Number of decimal places used to identify ties in
#'   `ranking`. Use `NULL` to disable rounding before tie detection.
#' @param na_rm Logical. If `TRUE`, incomplete pairs are removed. If `FALSE`,
#'   missing or non-finite values produce an error.
#'
#' @return
#' A long-format data frame with columns `p`, `share`, and `curve`. The `curve`
#' column identifies the Lorenz, dual Lorenz, concordance, or equality curve.
#'
#' @examples
#' actual <- c(2, 3, 5, 7)
#' predicted <- c(3, 7, 2, 5)
#'
#' curves <- rg_curves(
#'   reference = actual,
#'   ranking = predicted
#' )
#'
#' curves
#'
#' # Plot all four curves when ggplot2 is installed
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   curve_plot <- ggplot2::ggplot(
#'     curves,
#'     ggplot2::aes(
#'       x = p,
#'       y = share,
#'       linetype = curve
#'     )
#'   ) +
#'     ggplot2::geom_line(linewidth = 0.9) +
#'     ggplot2::coord_equal() +
#'     ggplot2::labs(
#'       title = "Rank Graduation Curves",
#'       x = "Cumulative proportion of observations",
#'       y = "Cumulative share",
#'       linetype = "Curve"
#'     ) +
#'     ggplot2::theme_minimal()
#'
#'   print(curve_plot)
#' }
#'
#' @seealso [lorenz_curve()], [dual_lorenz_curve()], [concordance_curve()],
#'   and [rga()].
#' @export
rg_curves <- function(reference, ranking, round_digits = 6L, na_rm = FALSE) {
  pair <- .prepare_pair(reference, ranking, na_rm = na_rm)
  reference <- pair$reference
  ranking <- pair$ranking

  l <- lorenz_curve(reference, include_origin = TRUE)
  d <- dual_lorenz_curve(reference, include_origin = TRUE)
  ccurve <- concordance_curve(
    reference,
    ranking,
    round_digits = round_digits,
    include_origin = TRUE
  )
  equality <- data.frame(p = l$p, share = l$p)

  bind_curve <- function(x, label) {
    data.frame(
      p = x$p,
      share = x$share,
      curve = label,
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(
    bind_curve(l, "Lorenz"),
    bind_curve(d, "Dual Lorenz"),
    bind_curve(ccurve, "Concordance"),
    bind_curve(equality, "Equality")
  )

  out$curve <- factor(
    out$curve,
    levels = c("Lorenz", "Dual Lorenz", "Concordance", "Equality")
  )

  rownames(out) <- NULL
  out
}
