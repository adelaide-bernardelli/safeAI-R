test_that("perfect, reverse, and tied rankings have expected values", {
  y <- 1:5
  expect_equal(rga(y, y), 1)
  expect_equal(rga(y, rev(y)), 0)
  expect_equal(rga(y, rep(1, 5)), 0.5)
})

test_that("the original example is reproduced", {
  y <- c(2, 3, 5, 7)
  yhat <- c(3, 7, 2, 5)
  expect_equal(rga(y, yhat), 8 / 17)
})

test_that("robustness and explainability use the same core correctly", {
  p <- c(0.1, 0.3, 0.7, 0.9)
  stressed <- c(0.15, 0.28, 0.72, 0.85)

  expect_equal(rgr(p, p), 1)
  expect_equal(rgr(p, stressed), rgx(p, stressed))
  expect_equal(rge(p, p), 0)
  expect_equal(rge(p, rev(p)), 1)
  expect_equal(rge_star(p, p), 1)
})

test_that("the metric is invariant to positive affine transformations of reference", {
  y <- c(-2, 0, 1, 5, 7)
  z <- c(3, 1, 5, 4, 2)
  expect_equal(rgx(y, z), rgx(10 + 4 * y, z))
})

test_that("ordered factors are accepted and unordered factors are rejected", {
  y <- ordered(c("low", "medium", "high"), levels = c("low", "medium", "high"))
  expect_equal(rga(y, y), 1)
  expect_error(rga(factor(c("a", "b")), 1:2), "unordered factor")
})

test_that("missing values are handled explicitly", {
  expect_error(rga(c(1, NA, 3), c(1, 2, 3)), "na_rm")
  expect_equal(rga(c(1, NA, 3), c(1, 2, 3), na_rm = TRUE), 1)
})

test_that("constant references are undefined", {
  expect_warning(value <- rga(rep(1, 5), 1:5), "undefined")
  expect_true(is.na(value))
})

test_that("the public API contains the documented lowercase functions", {
  exports <- getNamespaceExports("safeAI")
  expected_exports <- c(
    "concordance_curve",
    "dual_lorenz_curve",
    "lorenz_curve",
    "rga",
    "rge",
    "rge_star",
    "rg_curves",
    "rgr",
    "rgx"
  )

  expect_setequal(exports, expected_exports)
})
