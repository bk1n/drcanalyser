#' Normalise intensities for an IC50 curve
#'
#' `y = (y_i / y_nc) * 100`
#'
#' @param intensity Numeric vector of intensities representing drugged wells.
#' @param negative_control Average of DMSO-treated (NC-1) wells.
#' @return Numeric vector of normalised intensities.
#' @export
ic <- function(intensity, negative_control) {
    (intensity / negative_control) * 100
}

#' Normalise intensities for a GI50 curve
#'
#' `y = (y_i - y_0) / (y_nc - y_0) * 100`
#'
#' @param intensity Numeric vector of intensities representing drugged wells.
#' @param negative_control Average of DMSO-treated (NC-1) wells.
#' @param zero_control Average of untreated (NC-0) wells from the GI50 plate.
#' @return Numeric vector of normalised intensities.
#' @export
gi <- function(intensity, negative_control, zero_control) {
    ((intensity - zero_control) / (negative_control - zero_control)) * 100
}

#' Normalise intensities to growth rate
#'
#' Normalises data to growth rate, as in Hafner et al., 2016:
#' `y = 2^(log2(y_i/y_0) / log2(y_nc/y_0)) - 1`.
#'
#' @param intensity Numeric vector of intensities representing drugged wells.
#' @param negative_control Average of DMSO-treated (NC-1) wells.
#' @param zero_control Average of untreated (NC-0) wells from the GI50 plate.
#' @return Numeric vector of growth-rate normalised values.
#' @export
gr <- function(intensity, negative_control, zero_control) {
    (2^(log2(intensity / zero_control) / log2(negative_control / zero_control)) - 1) * 100
}
