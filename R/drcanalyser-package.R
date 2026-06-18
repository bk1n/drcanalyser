#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' Re-exported from \pkg{magrittr}. See \code{magrittr::\link[magrittr:pipe]{\%>\%}}.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

# Column / aesthetic names referenced via non-standard evaluation inside dplyr
# verbs and ggplot2 aes(); declared here so R CMD check does not flag them as
# undefined globals.
utils::globalVariables(c(
    "CONCS", "TRT_INTENSITY", "XCTRL", "X0", "BCKGRND",
    "TRT_INTENSITY_IC", "TRT_INTENSITY_GI", "TRT_INTENSITY_GR",
    "concs", "response", "assay_id", "se",
    "Prediction", "Upper", "Lower", "levels", "std_error", "units"
))
