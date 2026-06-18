# Shared fixtures for the test suite.
#
# The package bundles two real SparkControl exports in inst/extdata:
#   * main_plate.xlsx   - the treatment plate (6 rows x 10 cols of wells)
#   * growth_plate.xlsx - the matching day-zero / GI50 plate (3 rows x 8 cols)
# Together with the default GI50 layout they exercise the whole pipeline.

main_plate_path <- function() {
  system.file("extdata", "main_plate.xlsx", package = "drcanalyser")
}

growth_plate_path <- function() {
  system.file("extdata", "growth_plate.xlsx", package = "drcanalyser")
}

# Skip a test gracefully if the bundled example data isn't installed.
skip_if_no_example_plates <- function() {
  testthat::skip_if(
    main_plate_path() == "" || growth_plate_path() == "",
    "example plate data not available"
  )
}

# Run an expression while swallowing the package's cat()/print() chatter,
# progress messages and (numerical) warnings, returning its value.
quiet <- function(expr) {
  out <- NULL
  utils::capture.output(
    out <- suppressWarnings(suppressMessages(expr))
  )
  out
}

# A processed-plates object for a single GR-normalised replicate.
example_processed <- function(normalisation_method = "GR", ...) {
  quiet(process_plates(
    list(list(
      main_path = main_plate_path(),
      gi50_path = growth_plate_path(),
      plate_id = "p1",
      ...
    )),
    assay_id = "example",
    normalisation_method = normalisation_method
  ))
}
