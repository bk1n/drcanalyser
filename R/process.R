#' Process a single viability plate
#'
#' Normalises intensities from a viability assay given a plate layout and the
#' relevant plate paths.
#'
#' @param main_path Excel spreadsheet for the main treatment plate generated
#'   from SparkControl, see [format_standard_xl()] for details.
#' @param gi50_path If applicable, path to the corresponding GI50 plate
#'   generated from SparkControl.
#' @param plate_layout Optional path to an Excel spreadsheet with layout and
#'   concentrations, see [read_plate_layout()] for details. When `NULL`, a
#'   bundled default layout is used (GI50 or IC50 depending on `gi50_path`).
#' @param exclude Optional wells to exclude as a list of `c(row, col)` vectors,
#'   e.g. `list(c('A', 1))`.
#' @param plate_id Optional plate identifier; a random id is generated if absent.
#' @param assay_id Optional assay identifier; a random id is generated if absent.
#' @return A data.frame of concentrations and normalised treatment intensities.
#' @export
process_plate <- function(
    main_path,
    gi50_path = NULL,
    plate_layout = NULL,
    exclude = NULL,
    plate_id = NULL,
    assay_id = NULL) {
    if (is.null(plate_id)) {
        warning("plate_id not supplied, generating random plate_id")
        plate_id <- paste0(sample(LETTERS, 8, replace = TRUE), collapse = "")
    }
    if (is.null(assay_id)) {
        warning("assay_id not supplied, generating random assay_id")
        assay_id <- paste0(sample(LETTERS, 8, replace = TRUE), collapse = "")
    }

    # read plate layout excel
    r <- if (!is.null(plate_layout)) {
        read_plate_layout(plate_layout)
    } else if (!is.null(gi50_path)) {
        read_plate_layout(path = system.file("extdata", "plate_layout_gi50.xlsx", package = "drcanalyser"))
    } else {
        read_plate_layout(path = system.file("extdata", "plate_layout_ic50.xlsx", package = "drcanalyser"))
    }

    layout <- if (!is.null(exclude)) exclude_concentrations(r$layout, exclude) else r$layout
    concs <- r$concs
    gi50 <- r$gi50

    incl_gi50 <- if (!is.null(gi50)) TRUE else FALSE
    if (incl_gi50 & is.null(gi50)) stop("gi50_path supplied but gi50 is not included in plate layout!")

    # read main & GI50 excel
    main_intensity <- read_xl(main_path)
    gi50_intensity <- if (incl_gi50) read_xl(gi50_path) else NULL

    cat("Main intensity:\n")
    print(main_intensity)
    cat("\n")
    cat("GI50 intensity:\n")
    print(gi50_intensity)
    cat("\n")

    layout <- match_layout(layout, main_intensity)
    gi50 <- match_layout(gi50, gi50_intensity)
    concs <- match_layout(concs, main_intensity)

    # separate intensities into list of trt, neg_ctrl, bckgrnd
    intensity_lst <- list()
    conc_lst <- list()
    for (i in 1:nrow(layout)) {
        for (j in 1:ncol(layout)) {
            l <- layout[i, j]
            c <- as.numeric(concs[i, j])
            intens <- as.numeric(main_intensity[i, j])

            if (grepl("trt", l)) {
                if (grepl("exclude", l)) {
                    l <- gsub("exclude", "", l)
                    conc_lst[[l]] <- c(conc_lst[[l]], NA)
                    intensity_lst[[l]] <- c(intensity_lst[[l]], NA)
                } else {
                    conc_lst[[l]] <- c(conc_lst[[l]], as.numeric(c))
                    intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
                }
            } else {
                intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
            }
        }
    }

    # convert gi50 into list
    if (incl_gi50) {
        for (i in 1:nrow(gi50)) {
            for (j in 1:ncol(gi50)) {
                l <- gi50[i, j]
                intens <- as.numeric(gi50_intensity[i, j])

                if (l == "gi50" & !is.na(l)) {
                    intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
                }
            }
        }
    }

    # cat("Processed intensities:\n")
    # print(intensity_lst)
    # cat("\n")

    trt_df <- data.frame(intensity_lst[grepl("trt", names(intensity_lst))])

    conc_df <- data.frame(conc_lst)
    concs <- as.numeric(apply(conc_df, 1, function(x) {
        x <- unique(x)
        x <- x[!is.na(x)]

        stopifnot(length(x) <= 1)
        x
    }))

    df <- data.frame(
        ASSAY_ID = assay_id,
        PLATE_ID = plate_id,
        CONCS = concs,
        TRT_INTENSITY = rowMeans(trt_df, na.rm = T),
        XCTRL = mean(intensity_lst[["neg_ctrl"]], na.rm = T),
        X0 = mean(intensity_lst[["gi50"]], na.rm = T),
        BCKGRND = mean(intensity_lst[["bckgrnd"]], na.rm = T)
    )
    df <- df %>%
        dplyr::mutate(
            TRT_INTENSITY_IC = ic(TRT_INTENSITY, XCTRL),
            TRT_INTENSITY_GI = gi(TRT_INTENSITY, XCTRL, X0),
            TRT_INTENSITY_GR = gr(TRT_INTENSITY, XCTRL, X0)
        )

    cat("Processed plate:\n")
    print(utils::head(df))

    return(df)
}

#' Process a list of plates and fit a model
#'
#' Wrapper for [process_plate()], for processing lists of plates and fitting a
#' single dose-response model.
#'
#' @param path_list List of lists containing path information for the plates. In
#'   its most basic form, `list(list(main_path = ''), list(main_path = ''))`.
#' @param assay_id Optional, appends an `assay_id` to the returned processed
#'   plate.
#' @param normalisation_method One of `"IC"`, `"GI"`, `"GR"`; selects which
#'   normalisation column the model is fit on.
#' @return A list containing the combined `plate` data.frame and the fitted
#'   `model`.
#' @export
process_plates <- function(path_list, assay_id = NULL, normalisation_method = c("IC", "GI", "GR")) {
    all_plates <- lapply(path_list, function(p) {
        main_path <- if ("main_path" %in% names(p)) p$main_path else stop("Main path must be supplied!")

        p$assay_id <- assay_id

        plate <- do.call(process_plate, p)
        return(plate)
    })
    all_plates <- do.call(rbind, all_plates)

    nm <- normalisation_method
    if (length(nm) > 1) {
        nm <- nm[1]
        cat("Data is", nm, "normalised\n")
    }

    x <- all_plates[, c(paste0("TRT_INTENSITY_", nm), "CONCS")]
    model <- drc::drm(x, fct = drc::LL.3u(upper = 100))

    return(list(
        plate = all_plates,
        model = model
    ))
}
