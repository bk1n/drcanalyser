test_that("process_plate() normalises a plate into the expected tidy frame", {
  skip_if_no_example_plates()

  df <- quiet(process_plate(
    main_path = main_plate_path(),
    gi50_path = growth_plate_path(),
    plate_id = "p1",
    assay_id = "example"
  ))

  expect_s3_class(df, "data.frame")
  expect_true(all(
    c(
      "ASSAY_ID", "PLATE_ID", "CONCS", "TRT_INTENSITY", "XCTRL", "X0",
      "BCKGRND", "TRT_INTENSITY_IC", "TRT_INTENSITY_GI", "TRT_INTENSITY_GR"
    ) %in% colnames(df)
  ))
  # one row per concentration (12-point half-log dilution)
  expect_equal(nrow(df), 12)
  expect_equal(unique(df$ASSAY_ID), "example")
  expect_equal(unique(df$PLATE_ID), "p1")

  # concentrations are the half-log dilution series
  expect_setequal(
    round(sort(df$CONCS), 6),
    round(sort(get_halflog_doses(30, 0:11)), 6)
  )

  # the normalisation columns are exactly the documented transforms of the raw
  # treatment intensity against the plate's control means
  expect_equal(df$TRT_INTENSITY_IC, ic(df$TRT_INTENSITY, df$XCTRL))
  expect_equal(df$TRT_INTENSITY_GI, gi(df$TRT_INTENSITY, df$XCTRL, df$X0))
  expect_equal(df$TRT_INTENSITY_GR, gr(df$TRT_INTENSITY, df$XCTRL, df$X0))
})

test_that("process_plate() emits a warning when ids are not supplied", {
  skip_if_no_example_plates()

  # don't route through quiet() here: it suppresses warnings
  expect_warning(
    suppressMessages(utils::capture.output(
      process_plate(
        main_path = main_plate_path(),
        gi50_path = growth_plate_path(),
        assay_id = "example" # plate_id left out
      )
    )),
    "plate_id"
  )
})

test_that("process_plate() exclude= drops the named wells from the means", {
  skip_if_no_example_plates()

  base <- quiet(process_plate(
    main_path = main_plate_path(),
    gi50_path = growth_plate_path(),
    plate_id = "p1", assay_id = "example"
  ))
  excluded <- quiet(process_plate(
    main_path = main_plate_path(),
    gi50_path = growth_plate_path(),
    plate_id = "p1", assay_id = "example",
    exclude = list(c("B", 2), c("C", 2), c("D", 2))
  ))

  # same shape, but the excluded wells change at least one per-concentration mean
  expect_equal(nrow(base), nrow(excluded))
  expect_false(isTRUE(all.equal(base$TRT_INTENSITY, excluded$TRT_INTENSITY)))
})

test_that("process_plates() combines replicates and fits a drm model", {
  skip_if_no_example_plates()

  res <- quiet(process_plates(
    list(
      list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p1"),
      list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p2")
    ),
    assay_id = "example",
    normalisation_method = "GR"
  ))

  expect_named(res, c("plate", "model"))
  expect_s3_class(res$model, "drc")
  # two replicate plates -> 24 rows, two distinct plate ids
  expect_equal(nrow(res$plate), 24)
  expect_setequal(unique(res$plate$PLATE_ID), c("p1", "p2"))
})

test_that("process_plates() fits on the chosen normalisation column", {
  skip_if_no_example_plates()

  for (nm in c("IC", "GI", "GR")) {
    res <- quiet(process_plates(
      list(list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p1")),
      assay_id = "example",
      normalisation_method = nm
    ))
    # the model's response column is the selected TRT_INTENSITY_<nm>
    expect_equal(colnames(res$model$data)[2], paste0("TRT_INTENSITY_", nm))
  }
})

test_that("process_plates() errors when a main_path is missing", {
  expect_error(
    quiet(process_plates(list(list()), normalisation_method = "GR")),
    "Main path"
  )
})
