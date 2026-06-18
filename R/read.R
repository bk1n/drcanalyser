#' Read a fixed-offset SparkControl plate export
#'
#' Takes a standard format Excel output from SparkControl and returns a
#' data.frame of the plate. The plate format assumes 96-wells, has columns 1:12 and rownames A:H.
#'
#' @param path Path to the Excel file.
#' @return A data.frame of the plate with row labels A:H.
#' @export
format_standard_xl <- function(path) {
    df <- readxl::read_excel(path, skip = 43, n_max = 10) %>%
        as.data.frame() %>%
        tibble::column_to_rownames(1)
    return(df)
}

#' Read a plate by locating the `<>` marker
#'
#' Given an Excel path, searches through the lines to find `<>` (the top-left
#' corner of the plate) and reads the plate grid from there.
#'
#' @param path Path to the Excel file.
#' @return A data.frame of the plate, with column and row names taken from the
#'   plate header and first column.
#' @export
read_xl <- function(path) {
    df <- as.data.frame(readxl::read_excel(path, sheet = 1))

    plate_start <- which(df[, 1] == "<>") #
    x <- as.character(df[plate_start, ])

    row <- df[plate_start:nrow(df), 1]
    y <- c()
    for (val in row) {
        if (!is.na(val)) y <- c(y, val) else break
    }

    df <- df[(plate_start + 1):(plate_start + length(y) - 1), 2:length(x)]
    colnames(df) <- x[-1]
    rownames(df) <- y[-1]

    return(df)
}

#' Compute half-log serial dilution doses
#'
#' Calculates serial dilution doses for a half-log dilution, with `n_steps` and
#' a `top_dose`.
#'
#' @param top_dose Numeric, the highest dose. Defaults to 30.
#' @param n_steps Integer vector of dilution steps. Defaults to `0:11`.
#' @return Numeric vector of doses, one per step.
#' @export
get_halflog_doses <- function(top_dose = 30, n_steps = 0:11) {
    return(sapply(n_steps, function(n) top_dose / (10^(0.5 * n))))
}
