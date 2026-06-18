test_that("get_halflog_doses() produces a half-log serial dilution", {
  doses <- get_halflog_doses(top_dose = 30, n_steps = 0:11)

  expect_length(doses, 12)
  # first dose is the top dose
  expect_equal(doses[1], 30)
  # each step is a half-log (sqrt(10)) dilution of the previous
  ratios <- doses[-length(doses)] / doses[-1]
  expect_equal(ratios, rep(sqrt(10), length(ratios)))
  # strictly decreasing
  expect_true(all(diff(doses) < 0))
})

test_that("read_xl() locates the <> marker and reads the treatment plate grid", {
  skip_if_no_example_plates()

  plate <- read_xl(main_plate_path())

  expect_s3_class(plate, "data.frame")
  # the main plate occupies rows B-G and columns 2-11
  expect_equal(dim(plate), c(6, 10))
  expect_setequal(rownames(plate), c("B", "C", "D", "E", "F", "G"))
  expect_setequal(colnames(plate), as.character(2:11))
})

test_that("read_xl() reads the smaller day-zero plate", {
  skip_if_no_example_plates()

  plate <- read_xl(growth_plate_path())

  expect_s3_class(plate, "data.frame")
  # the growth plate occupies rows B-D and columns 2-9
  expect_equal(dim(plate), c(3, 8))
  expect_setequal(rownames(plate), c("B", "C", "D"))
})
