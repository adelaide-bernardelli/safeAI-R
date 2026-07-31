#' General Rank Graduation Metric
#'
#' Computes the normalized rank-concordance measure between a reference vector
#' and a second vector that induces its ordering.
#'
#' @details
#' `rgx()` is the general computational engine underlying Rank Graduation
#' Accuracy (`rga()`), Rank Graduation Robustness (`rgr()`), and Rank
#' Graduation Explainability (`rge()`).
#'
#' The function orders `reference` according to `ranking` and compares the
#' resulting concordance arrangement with two benchmarks:
#'
#' - the increasing ordering of `reference`, corresponding to perfect
#'   concordance;
#' - the decreasing ordering of `reference`, corresponding to perfect reverse
#'   concordance.
#'
#' The normalized result lies between zero and one:
#'
#' - `RGX = 1` indicates perfect concordance;
#' - `RGX = 0` indicates perfect reverse ordering;
#' - `RGX = 0.5` is the neutral rank-ordering benchmark. A random ordering has
#'   expected value `0.5`, and the same value is obtained when all ranking
#'   values are tied.
#'
#' Ties in `ranking` are handled by replacing the corresponding reference
#' values with their tie-group mean before the concordance arrangement is
#' calculated.
#'
#' @param reference Numeric vector, logical vector, or ordered factor whose
#'   values define the increasing and decreasing benchmark orderings.
#' @param ranking Numeric vector, logical vector, or ordered factor that induces
#'   the ordering of `reference`.
#' @param round_digits Number of decimal places used to identify ties in
#'   `ranking`. The default, `6`, reproduces the original implementation. Use
#'   `NULL` to disable rounding before tie detection.
#' @param na_rm Logical. If `TRUE`, incomplete pairs are removed. If `FALSE`,
#'   missing or non-finite values produce an error.
#'
#' @return
#' A numeric scalar in the interval from zero to one. If `reference` is
#' constant, the metric is undefined and `NA` is returned with a warning.
#'
#' @examples
#' reference <- c(2, 3, 5, 7)
#' ranking <- c(3, 7, 2, 5)
#'
#' # Partially concordant ordering
#' rgx(reference, ranking)
#'
#' # Perfect concordance
#' rgx(reference, reference)
#'
#' # Perfect reverse ordering
#' rgx(reference, rev(reference))
#'
#' # All ranking values are tied
#' rgx(reference, rep(1, length(reference)))
#'
#' # Missing pairs can be removed explicitly
#' rgx(
#'   reference = c(2, 3, NA, 7),
#'   ranking = c(3, 7, 2, 5),
#'   na_rm = TRUE
#' )
#'
#' @seealso
#' [rga()], [rgr()], and [rge()] for the accuracy, robustness, and
#' explainability interpretations; [rg_curves()] for the associated cumulative
#' curves.
#'
#' @export
rgx <- function(reference, ranking, round_digits = 6L, na_rm = FALSE) {
  pair <- .prepare_pair(reference, ranking, na_rm = na_rm)
  reference <- pair$reference
  ranking <- pair$ranking
  round_digits <- .validate_round_digits(round_digits)

  key <- if (is.null(round_digits)) ranking else round(ranking, round_digits)

  # Integer tie groups ordered from the smallest to the largest ranking value.
  groups <- match(key, sort(unique(key)))

  # Tied ranking values receive the mean of the corresponding reference values.
  tied_reference <- stats::ave(reference, groups, FUN = mean)
  ordered_reference <- tied_reference[order(groups, seq_along(groups))]

  index <- seq_along(reference)
  concordance <- sum(index * ordered_reference)
  decreasing <- sum(index * sort(reference, decreasing = TRUE))
  increasing <- sum(index * sort(reference, decreasing = FALSE))
  denominator <- increasing - decreasing

  scale_reference <- max(1, max(abs(reference)))
  if (
    abs(denominator) <=
      .Machine$double.eps * scale_reference * length(reference)^2
  ) {
    warning(
      "The rank graduation metric is undefined for a constant reference vector.",
      call. = FALSE
    )
    return(NA_real_)
  }

  .clamp_unit((concordance - decreasing) / denominator)
}


#' Rank Graduation Accuracy
#'
#' Measures the rank concordance between observed outcomes and model
#' predictions or scores.
#'
#' @details
#' Rank Graduation Accuracy, or RGA, evaluates how well the ordering induced
#' by predicted values reproduces the ordering of observed outcomes. It focuses
#' on ranking rather than on the numerical distance between actual and
#' predicted values.
#'
#' RGA lies between zero and one:
#'
#' - `RGA = 1` indicates perfect rank concordance. Observations with larger
#'   outcomes also receive larger predicted scores.
#' - `RGA = 0` indicates a completely reversed ordering.
#' - `RGA = 0.5` is the neutral rank-discrimination benchmark. A random ordering
#'   has expected value `0.5`, and the same value is obtained when all predicted
#'   values are tied.
#'
#' Values above `0.5` indicate positive rank concordance. Values below `0.5`
#' indicate that the predicted ordering is closer to the reverse of the
#' observed ordering.
#'
#' Tied predicted values are handled by averaging the observed outcomes within
#' each prediction-tie group before the graduation measure is calculated.
#'
#' Ordered factors are supported and are converted internally to their ordered
#' integer scores. The direction of their levels therefore determines the
#' direction of the ranking.
#'
#' @param actual Observed outcomes supplied as a numeric vector, logical vector,
#'   or ordered factor.
#' @param predicted Numeric vector of model predictions or scores.
#' @param round_digits Number of decimal places used when identifying ties in
#'   `predicted`. Use `NULL` to disable rounding.
#' @param na_rm Logical. When `TRUE`, incomplete pairs are removed before the
#'   metric is calculated.
#'
#' @return
#' A numeric scalar in the interval from zero to one.
#'
#' @examples
#' actual <- c(2, 3, 5, 7)
#'
#' # A partially concordant ranking
#' predicted <- c(3, 7, 2, 5)
#' rga(actual, predicted)
#'
#' # Perfect concordance
#' rga(actual, actual)
#'
#' # Completely reversed ordering
#' rga(actual, rev(actual))
#'
#' # No rank discrimination: all predictions are tied
#' rga(actual, rep(1, length(actual)))
#'
#' # Ordered factors are also supported
#' actual_ord <- ordered(
#'   c("low", "medium", "high", "high"),
#'   levels = c("low", "medium", "high")
#' )
#' predicted_ord <- c(1.2, 2.1, 2.8, 3.4)
#' rga(actual_ord, predicted_ord)
#'
#' @seealso
#' [rgx()] for the general metric engine and [rg_curves()] for the cumulative
#' curves underlying the measure.
#'
#' @export
rga <- function(
  actual,
  predicted,
  round_digits = 6L,
  na_rm = FALSE
) {
  rgx(
    reference = actual,
    ranking = predicted,
    round_digits = round_digits,
    na_rm = na_rm
  )
}


#' Rank Graduation Robustness
#'
#' Measures the stability of a baseline prediction ranking under a
#' user-defined perturbation or stress scenario.
#'
#' @details
#' Rank Graduation Robustness, or RGR, compares the ordering induced by a
#' baseline vector of predictions with the ordering induced by a second vector
#' obtained under a user-defined perturbation or stress scenario.
#'
#' The package does not prescribe how that scenario must be generated. The
#' second prediction vector may result from altered training data, modified
#' test inputs, parameter stress, alternative preprocessing, model refitting,
#' prediction noise, or another design selected by the researcher.
#'
#' RGR lies between zero and one:
#'
#' - `RGR = 1` indicates that the perturbed predictions preserve the baseline
#'   ordering perfectly.
#' - `RGR = 0` indicates that the perturbed predictions completely reverse the
#'   baseline ordering.
#' - `RGR = 0.5` is the neutral rank-stability benchmark. A random perturbed
#'   ordering has expected value `0.5`, and the same value is obtained when all
#'   perturbed predictions are tied.
#'
#' Values above `0.5` indicate that the baseline ranking is preserved more than
#' it is reversed. Larger values therefore indicate greater rank stability
#' under the selected scenario.
#'
#' RGR evaluates the consequences of the scenario supplied by the user. Its
#' substantive interpretation always depends on how that scenario was
#' constructed.
#'
#' @param predicted Numeric vector of baseline model predictions or scores.
#' @param perturbed Numeric vector of predictions or scores obtained under the
#'   user-defined perturbation or stress scenario.
#' @param round_digits Number of decimal places used when identifying ties in
#'   `perturbed`. Use `NULL` to disable rounding.
#' @param na_rm Logical. When `TRUE`, incomplete pairs are removed before the
#'   metric is calculated.
#'
#' @return
#' A numeric scalar in the interval from zero to one.
#'
#' @examples
#' baseline <- c(1.2, 2.5, 3.1, 4.8, 5.6)
#'
#' # Values change but the ranking is preserved
#' stressed_same_order <- c(1.0, 2.8, 3.0, 4.5, 6.1)
#' rgr(baseline, stressed_same_order)
#'
#' # Part of the ordering changes
#' stressed_partial <- c(1.0, 3.2, 2.7, 5.1, 4.8)
#' rgr(baseline, stressed_partial)
#'
#' # The perturbed ranking is completely reversed
#' rgr(baseline, rev(baseline))
#'
#' # The perturbed predictions contain no rank discrimination
#' rgr(baseline, rep(0, length(baseline)))
#'
#' @seealso
#' [rgx()] for the general metric engine, [rga()] for accuracy, and [rge()] for
#' explainability.
#'
#' @export
rgr <- function(
  predicted,
  perturbed,
  round_digits = 6L,
  na_rm = FALSE
) {
  rgx(
    reference = predicted,
    ranking = perturbed,
    round_digits = round_digits,
    na_rm = na_rm
  )
}


#' Rank Graduation Explainability
#'
#' Measures the change in a full-model prediction ranking after one or more
#' predictors are removed.
#'
#' @details
#' Rank Graduation Explainability, or RGE, compares predictions from a full
#' model with predictions from a reduced model.
#'
#' First, `rge_star()` measures the rank concordance retained by the reduced
#' model:
#'
#' \deqn{RGE^* = RGX(\widehat{y}_{full}, \widehat{y}_{reduced}).}
#'
#' The associated rank loss is then:
#'
#' \deqn{RGE = 1 - RGE^*.}
#'
#' RGE lies between zero and one:
#'
#' - `RGE = 0` indicates that the reduced model preserves the full-model
#'   ordering perfectly. Removing the predictor or predictor group produces no
#'   rank loss.
#' - `RGE = 1` indicates that the reduced model completely reverses the
#'   full-model ordering.
#' - `RGE = 0.5` is the neutral rank-loss benchmark. It corresponds to retained
#'   concordance of `0.5` and is obtained, for example, when all reduced-model
#'   predictions are tied.
#'
#' Values closer to zero indicate that the reduced model preserves most of the
#' full-model ranking. Larger values indicate a greater change in the ranking
#' after the variable or variable group is removed.
#'
#' RGE is model-specific and depends on the selected reduced-model design. It
#' should be interpreted as predictive rank importance rather than as a causal
#' effect.
#'
#' @param full Numeric vector of predictions from the full model.
#' @param reduced Numeric vector of predictions from the reduced model.
#' @param round_digits Number of decimal places used when identifying ties in
#'   `reduced`. Use `NULL` to disable rounding.
#' @param na_rm Logical. When `TRUE`, incomplete pairs are removed before the
#'   metric is calculated.
#'
#' @return
#' `rge_star()` returns the rank concordance retained by the reduced model.
#' `rge()` returns one minus that retained concordance. Both values lie in the
#' interval from zero to one.
#'
#' @examples
#' full <- c(1.2, 2.5, 3.1, 4.8, 5.6)
#'
#' # The reduced model preserves the full-model ordering
#' reduced_same_order <- c(10, 20, 30, 40, 50)
#' rge_star(full, reduced_same_order)
#' rge(full, reduced_same_order)
#'
#' # The reduced model changes part of the ranking
#' reduced_partial <- c(1.0, 3.2, 2.7, 5.1, 4.8)
#' rge(full, reduced_partial)
#'
#' # The reduced model reverses the full-model ordering
#' rge(full, rev(full))
#'
#' # The reduced model contains no rank discrimination
#' rge(full, rep(0, length(full)))
#'
#' # RGE is the complement of retained rank concordance
#' value_star <- rge_star(full, reduced_partial)
#' value <- rge(full, reduced_partial)
#' all.equal(value, 1 - value_star)
#'
#' @seealso
#' [rgx()] for the general metric engine, [rga()] for accuracy, and [rgr()] for
#' robustness.
#'
#' @export
rge_star <- function(full, reduced, round_digits = 6L, na_rm = FALSE) {
  rgx(
    reference = full,
    ranking = reduced,
    round_digits = round_digits,
    na_rm = na_rm
  )
}


#' @rdname rge_star
#' @export
rge <- function(full, reduced, round_digits = 6L, na_rm = FALSE) {
  .clamp_unit(
    1 - rge_star(
      full,
      reduced,
      round_digits = round_digits,
      na_rm = na_rm
    )
  )
}
