#' Validate a plate layout
#'
#' Checks the validity of a plate layout read from a spreadsheet, confirming the
#' required well types (`bckgrnd`, `neg_ctrl`, `trt`) are present.
#'
#' @param layout Layout data.frame (rows = plate rows, first column = labels).
#' @return `TRUE` if all required elements are present, otherwise `FALSE`.
#'   Prints a summary of well contents.
#' @keywords internal
check_plate_layout <- function(layout) {
    mat <- as.matrix(layout)[, -1]
    colnames(mat) <- NULL
    rownames(mat) <- NULL

    contents <- table(mat)
    contents_vec <- as.vector(contents)
    names(contents_vec) <- names(contents)

    cat("Number of wells included in layout:\n")
    print(contents_vec)

    clean_contents_vec <- gsub("::n[0-9]", "", names(contents_vec))
    check <- c("bckgrnd", "neg_ctrl", "trt") %in% clean_contents_vec
    names(check) <- c("bckgrnd", "neg_ctrl", "trt")

    if (all(check)) {
        cat("All necessary elements found in layout\n")
        return(TRUE)
    } else {
        cat("Missing necessary elements in layout:\n")
        print(check)
        return(FALSE)
    }
}

#' Read a standardised plate layout workbook
#'
#' Reads a standardised layout spreadsheet from `path`. The workbook is expected
#' to contain `layout` and `concs` sheets, and optionally a `gi50` sheet.
#'
#' @param path Path to the layout Excel workbook.
#' @return A list with elements `layout`, `concs`, and `gi50` (`NULL` if the
#'   `gi50` sheet is absent).
#' @export
read_plate_layout <- function(path) {
    layout <- readxl::read_excel(path, sheet = "layout")
    concs <- readxl::read_excel(path, sheet = "concs")

    gi50 <- tryCatch(
        {
            gi50 <- readxl::read_excel(path, sheet = "gi50")
        },
        error = function(e) {
            message("gi50 not available: \n", e)
            return(NULL)
        }
    )

    layout_df <- as.data.frame(layout)[, -1]
    rownames(layout_df) <- layout[[1]]

    stopifnot(check_plate_layout(layout_df))

    concs_df <- as.data.frame(concs)[, -1]
    rownames(concs_df) <- concs[[1]]

    if (!is.null(gi50)) {
        gi50_df <- as.data.frame(gi50)[, -1]
        rownames(gi50_df) <- gi50[[1]]
    } else {
        gi50_df <- NULL
    }

    return(list(layout = layout_df, concs = concs_df, gi50 = gi50_df))
}

#' Exclude concentrations from a layout
#'
#' Marks wells to be removed from a layout by appending `"exclude"` to their
#' label.
#'
#' @param layout Layout data.frame.
#' @param exclude List of `c(row, col)` vectors identifying wells to exclude.
#' @return The layout with the specified wells marked.
#' @keywords internal
exclude_concentrations <- function(layout, exclude) {
    for (ex in exclude) {
        layout[ex[1], ex[2]] <- paste0(layout[ex[1], ex[2]], "exclude")
    }
    return(layout)
}

#' Match a layout plate to a main plate
#'
#' Subsets the layout to the rows and columns shared with the main plate.
#'
#' @param layout Layout data.frame.
#' @param main Main plate data.frame.
#' @return The layout restricted to the rows/columns present in `main`.
#' @keywords internal
match_layout <- function(layout, main) {
    layout <- layout[rownames(layout) %in% rownames(main), colnames(layout) %in% colnames(main)]
    return(layout)
}
