test_that("curve helpers include the origin and end at one", {
  y <- c(1, 2, 3, 4)
  l <- lorenz_curve(y)
  d <- dual_lorenz_curve(y)
  ccurve <- concordance_curve(y, y)

  expect_equal(l[1, ], data.frame(p = 0, share = 0))
  expect_equal(tail(l$share, 1), 1)
  expect_equal(tail(d$share, 1), 1)
  expect_equal(ccurve, l)
})

test_that("rg_curves returns four labeled curves", {
  out <- rg_curves(1:4, c(1, 3, 2, 4))
  expect_setequal(levels(out$curve), c("Lorenz", "Dual Lorenz", "Concordance", "Equality"))
  expect_equal(nrow(out), 4 * 5)
})
