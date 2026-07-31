# safeAI

`safeAI` is an R package for **univariate Rank Graduation metrics** used in the SAFE artificial-intelligence framework. It provides reusable, documented, and tested functions while keeping model fitting and evaluation-scenario design separate from metric calculation.

## Included functions

- `rga()`: Rank Graduation Accuracy, comparing observed and predicted outcomes.
- `rgr()`: Rank Graduation Robustness, comparing baseline predictions with predictions obtained under a user-defined robustness scenario.
- `rge()` and `rge_star()`: Rank Graduation Explainability, comparing full- and reduced-model predictions.
- `rgx()`: the common rank-graduation engine underlying the metrics.
- `lorenz_curve()`, `dual_lorenz_curve()`, `concordance_curve()`, and `rg_curves()`: curve-construction helpers.

The functions accept continuous, binary, logical, and ordered-factor outcomes. Tied values in the ranking vector are handled by averaging the corresponding reference values before the concordance ordering is formed.

## Installation

Install the source archive with:

```r
install.packages(
  "safeAI_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

For development from the extracted source folder:

```r
install.packages(c("devtools", "roxygen2", "testthat"))

devtools::document("safeAI")
devtools::test("safeAI")
devtools::check("safeAI")
devtools::install("safeAI")
```

From inside the RStudio project, omit the package path:

```r
devtools::document()
devtools::test()
devtools::check()
devtools::install()
```

## Accuracy

```r
library(safeAI)

y <- c(2, 3, 5, 7)
yhat <- c(3, 7, 2, 5)

rga(y, yhat)
# 0.4705882
```

The ranking boundaries are:

```r
rga(1:5, 1:5)  # 1: perfect concordance
rga(1:5, 5:1)  # 0: perfect reverse ordering
```

When all predictions are tied, the metric equals the random-ranking benchmark:

```r
rga(1:5, rep(1, 5))
# 0.5
```

## Robustness

`rgr()` compares two prediction vectors. The package does not prescribe how the second vector must be generated. It may come from altered training data, modified inputs, a different model specification, a parameter stress, or another robustness experiment chosen by the analyst.

```r
pred_baseline <- c(0.12, 0.31, 0.55, 0.73, 0.91)
pred_stressed <- c(0.15, 0.28, 0.58, 0.70, 0.88)

rgr(pred_baseline, pred_stressed)
```

## Explainability

```r
pred_full <- c(0.1, 0.3, 0.7, 0.9)
pred_without_x1 <- c(0.2, 0.4, 0.6, 0.8)

rge(pred_full, pred_without_x1)
```

`rge_star()` measures retained rank similarity. `rge()` reports its complement:

```text
rge(full, reduced) = 1 - rge_star(full, reduced)
```

A value close to zero means that removing the variable leaves the prediction ranking almost unchanged. A larger value indicates a larger contribution of the removed variable or group.

## Ordinal outcomes

Ordered factors can be supplied directly as observed outcomes:

```r
actual <- ordered(
  c("low", "medium", "high", "medium"),
  levels = c("low", "medium", "high")
)

predicted_score <- c(1.1, 2.2, 2.8, 1.9)

rga(actual, predicted_score)
```

For ordinal models that return class probabilities, first convert each probability vector to a one-dimensional ordinal score using a clearly stated class-scoring convention, and then pass that score to the SAFE functions.

## Curves

```r
curves <- rg_curves(y, yhat)

library(ggplot2)

ggplot(curves, aes(p, share, linetype = curve)) +
  geom_line(linewidth = 0.9) +
  coord_equal() +
  theme_minimal()
```
