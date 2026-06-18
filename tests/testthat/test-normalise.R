test_that("ic() scales intensity relative to the negative control", {
  # intensity == control -> 100%, intensity == 0 -> 0%
  expect_equal(ic(100, 100), 100)
  expect_equal(ic(0, 100), 0)
  expect_equal(ic(50, 200), 25)
  # vectorised over intensities
  expect_equal(ic(c(50, 100, 200), 100), c(50, 100, 200))
})

test_that("gi() anchors 0% at the day-zero baseline and 100% at the control", {
  # at the zero (day-0) baseline the response is 0%
  expect_equal(gi(500, 1000, 500), 0)
  # at the negative control the response is 100%
  expect_equal(gi(1000, 1000, 500), 100)
  # with a zero baseline it reduces to the IC formula
  expect_equal(gi(250, 1000, 0), ic(250, 1000))
})

test_that("gr() follows Hafner et al. (2016) growth-rate normalisation", {
  # intensity == control -> 100, intensity == zero baseline -> 0
  expect_equal(gr(1000, 1000, 250), 100)
  expect_equal(gr(250, 1000, 250), 0)
  # explicit check against the closed-form definition
  expect_equal(
    gr(500, 1000, 250),
    (2^(log2(500 / 250) / log2(1000 / 250)) - 1) * 100
  )
})
