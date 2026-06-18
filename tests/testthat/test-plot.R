test_that("plot_drc() returns a ggplot and an ED10/50/90 stats table", {
  skip_if_no_example_plates()

  res <- example_processed("GR")
  out <- quiet(plot_drc(list(res)))

  expect_named(out, c("plot", "stats"))
  expect_s3_class(out$plot, "ggplot")

  stats <- out$stats
  expect_s3_class(stats, "data.frame")
  # one row per ED level for the single condition
  expect_equal(nrow(stats), 3)
  expect_setequal(stats$levels, c("e:1:10", "e:1:50", "e:1:90"))
  expect_equal(unique(stats$assay_id), "example")
  # stat-type column is inferred from the normalisation (GR here)
  expect_true("GR" %in% colnames(stats))
  expect_true(all(c("assay_id", "levels", "std_error", "units") %in% colnames(stats)))
})

test_that("plot_drc() names the stat column after the normalisation method", {
  skip_if_no_example_plates()

  out_ic <- quiet(plot_drc(list(example_processed("IC"))))
  expect_true("IC" %in% colnames(out_ic$stats))

  # IC50 (the e:1:50 level) should be a finite, positive concentration
  ic50 <- out_ic$stats$IC[out_ic$stats$levels == "e:1:50"]
  expect_true(is.finite(ic50))
  expect_gt(ic50, 0)
})

test_that("plot_drc() overlays multiple conditions in one stats table", {
  skip_if_no_example_plates()

  a <- quiet(process_plates(
    list(list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p1")),
    assay_id = "cond_A", normalisation_method = "GR"
  ))
  b <- quiet(process_plates(
    list(list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p1")),
    assay_id = "cond_B", normalisation_method = "GR"
  ))

  out <- quiet(plot_drc(list(a, b)))
  expect_setequal(unique(out$stats$assay_id), c("cond_A", "cond_B"))
  expect_equal(nrow(out$stats), 6)
})

test_that("plot_drc(plot_mean = TRUE) collapses replicates without error", {
  skip_if_no_example_plates()

  res <- quiet(process_plates(
    list(
      list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p1"),
      list(main_path = main_plate_path(), gi50_path = growth_plate_path(), plate_id = "p2")
    ),
    assay_id = "example", normalisation_method = "GR"
  ))

  out <- quiet(plot_drc(list(res), plot_mean = TRUE))
  expect_s3_class(out$plot, "ggplot")
})

test_that("save_results() writes an pdf figure and a CSV of stats", {
  skip_if_no_example_plates()

  out <- quiet(plot_drc(list(example_processed("GR"))))

  save_dir <- withr::local_tempdir()
  quiet(save_results(out, save_folder = save_dir, append_file_name = "test"))

  expect_true(file.exists(file.path(save_dir, "drc_plot_test.pdf")))
  expect_true(file.exists(file.path(save_dir, "drc_stats_test.csv")))

  written <- utils::read.csv(file.path(save_dir, "drc_stats_test.csv"))
  expect_equal(nrow(written), nrow(out$stats))
})
