default_layout <- function(which) {
  system.file("extdata", which, package = "drcanalyser")
}

test_that("read_plate_layout() reads the default GI50 workbook with a gi50 sheet", {
  skip_if(default_layout("plate_layout_gi50.xlsx") == "", "default layout missing")

  r <- quiet(read_plate_layout(default_layout("plate_layout_gi50.xlsx")))

  expect_named(r, c("layout", "concs", "gi50"))
  expect_s3_class(r$layout, "data.frame")
  expect_s3_class(r$concs, "data.frame")
  # the GI50 layout carries a day-zero plate map
  expect_false(is.null(r$gi50))
  # layout and concs describe the same well grid
  expect_equal(dim(r$layout), dim(r$concs))
})

test_that("read_plate_layout() omits gi50 for the IC50 workbook", {
  skip_if(default_layout("plate_layout_ic50.xlsx") == "", "default layout missing")

  r <- quiet(read_plate_layout(default_layout("plate_layout_ic50.xlsx")))

  expect_null(r$gi50)
})

test_that("check_plate_layout() requires bckgrnd, neg_ctrl and trt wells", {
  # check_plate_layout() drops the first column, so include a dummy label column.
  valid <- data.frame(
    .label = c("r1", "r2"),
    w1 = c("bckgrnd", "trt::n1"),
    w2 = c("neg_ctrl", "trt::n2"),
    stringsAsFactors = FALSE
  )
  expect_true(quiet(drcanalyser:::check_plate_layout(valid)))

  # missing neg_ctrl -> invalid
  invalid <- data.frame(
    .label = c("r1", "r2"),
    w1 = c("bckgrnd", "trt::n1"),
    w2 = c("bckgrnd", "trt::n2"),
    stringsAsFactors = FALSE
  )
  expect_false(quiet(drcanalyser:::check_plate_layout(invalid)))
})
