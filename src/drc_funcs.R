library(drc)
library(tidyverse)

# Takes a standard format Excel output from SparkControl, and returns a df of the plate
# Plate format has columns 1:12, and rownames A:H
format_standard_xl <- function(path) {
    df <- readxl::read_excel(path, skip = 43, n_max = 10) %>%
        as.data.frame() %>%
        column_to_rownames(1)
    return(df)
}

# Calculates serial dilution doses for a halflog dilution, with n_steps and a top_dose
get_halflog_doses <- function(top_dose = 30, n_steps = 0:11) {
    return(sapply(n_steps, function(n) top_dose / (10^(0.5 * n))))
}

# Checks validity of plate layout from spreadsheet
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

# read in standardised layout spreadsheet from path
read_plate_layout <- function(path = "plate_layout.xlsx") {
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

# mark concentrations for exclusion from data.frame
# exclude should be an integer or vector of integers, bottom dose is 1
# returns data.frame with exclude column containing 'exclude' flag
exclude_concentrations <- function(df, exclude) {
    # Ensure 'concs' column exists
    if (!"concs" %in% names(df)) {
        stop("Data frame must contain a 'concs' column.")
    }

    if (!is.null(exclude)) {
        df$exclude <- if_else(1:nrow(df) %in% exclude, "exclude", NA)
    } else {
        df$exclude <- NA
    }

    return(df)
}

# Normalise intensities from viability assay given plate_layout and path
# Args:
# - main_path: Excel spreadsheet for the main treatment plate generated from SparkControl, see format_standard_xl for details
# - gi50_path: if applicable, path to corresponding gi50 plate generated from SparkControl
# - plate_layout: optional, path to Excel spreadsheet with layout and concentrations, see read_plate_layout for details
# - exclude: optional, concentration ranges to be excluded, increasing order only, list of vectors (list(c(3,5), c(15,20)))
# Returns:
# - df: data.frame of concs, normalised treatment intensities
process_plate <- function(main_path, gi50_path = NULL, plate_layout = NULL, exclude = NULL) {
    r <- if (!is.null(plate_layout)) read_plate_layout(plate_layout) else read_plate_layout()

    layout <- r$layout
    concs <- r$concs
    gi50 <- r$gi50

    incl_gi50 <- if (!is.null(gi50)) TRUE else FALSE
    if (incl_gi50 & is.null(gi50)) stop("gi50_path supplied but gi50 is not included in plate layout!")

    main_intensity <- format_standard_xl(main_path)
    gi50_intensity <- if (incl_gi50) format_standard_xl(gi50_path) else NULL

    # separate intensities into list of trt, neg_ctrl, bckgrnd
    intensity_lst <- list()
    conc_lst <- list()
    for (i in 1:nrow(layout)) {
        for (j in 1:ncol(layout)) {
            l <- layout[i, j]
            c <- concs[i, j]
            intens <- main_intensity[i, j]

            intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
            if (grepl("trt", l)) {
                conc_lst[[l]] <- c(conc_lst[[l]], as.numeric(c))
            }
        }
    }

    # convert gi50 into list
    if (incl_gi50) {
        for (i in 1:nrow(gi50)) {
            for (j in 1:ncol(gi50)) {
                l <- gi50[i, j]
                intens <- gi50_intensity[i, j]

                if (l == "gi50" & !is.na(l)) {
                    intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
                }
            }
        }
    }

    # check all concentrations are identical for all repeats, merge
    all_ident <- all(sapply(conc_lst[-1], function(x) identical(conc_lst[[1]], x)))
    if (!all_ident) stop("Concentrations are not the same for all repeats, check layout.")
    concs_merged <- conc_lst[[1]]

    # take means of neg ctrls and backgrounds
    mean_gi50 <- if (incl_gi50) mean(intensity_lst$gi50) else NULL

    bckgrnd <- mean(intensity_lst[["bckgrnd"]])
    neg_ctrl <- if (incl_gi50) mean(intensity_lst[["neg_ctrl"]]) - mean_gi50 - bckgrnd else mean(intensity_lst[["neg_ctrl"]]) - bckgrnd

    # normalise treatment intensities
    if (incl_gi50) {
        trt_int <- sapply(intensity_lst[grepl("trt", names(intensity_lst))], function(x) x - mean_gi50 - bckgrnd)
    } else {
        trt_int <- sapply(intensity_lst[grepl("trt", names(intensity_lst))], function(x) x - bckgrnd)
    }
    trt_int_mean <- rowMeans(trt_int)
    trt_int_se <- apply(trt_int, 1, function(row) sd(row) / sqrt(length(row)))
    trt_int_norm_percmax <- (trt_int_mean / neg_ctrl) * 100

    df <- data.frame(
        concs = concs_merged,
        log_concs = log(concs_merged),
        trt_int_mean = trt_int_mean,
        trt_int_se = trt_int_se,
        trt_int_norm_percmax = trt_int_norm_percmax
    )

    df <- exclude_concentrations(df, exclude)

    if (incl_gi50) cat("Returning GI50\n") else cat("Returning IC50\n")

    return(df)
}

# Returns ggplot given a processed plate dataframe, with log conc on x and norm intensity on y
# Args:
# - processed_plate: data.frame from `process_plate()`
# - model: drc model object, for plotting model fit on ggplot
# - units: one of 'uM', 'nM', defaults to 'uM', set to NULL for no units
plot_drc <- function(
    processed_plate,
    model = NULL,
    exclude = FALSE,
    xlabs = "Concentration",
    units = "µM",
    ylabs = "Normalised Response (%)",
    stat_type = "GI50",
    title = NULL) {
    if (!is.null(model)) {
        # plot model fit
        newdata_df <- data.frame(
            concs = 10^seq(log10(max(processed_plate$concs)), log10(min(processed_plate$concs)), length.out = 200)
        )
        preds <- as.data.frame(predict(model, newdata = newdata_df, interval = "confidence"))
        preds$concs <- newdata_df$concs

        # get gi50 / ic50
        stat <- as.data.frame(drc::ED(model, respLev = c(10, 50, 90)))
        stat$levels <- if (stat_type == "GI50") c("GI[10]", "GI[50]", "GI[90]") else c("IC[10]", "IC[50]", "IC[90]")
        stat$y <- c(100, 90, 80)
        stat$label <- paste0(stat$levels, " ", "==", " '", signif(stat$Estimate, 3), units, "'")
    } else {
        newdata_df <- processed_plate
    }

    if (exclude) {
        processed_plate <- processed_plate[is.na(processed_plate$exclude), ]
    }

    df_points <- data.frame(
        concs = processed_plate$concs,
        response = processed_plate$trt_int_norm_percmax
    )

    df_drc <- data.frame(
        concs = preds$concs,
        preds = preds$Prediction,
        lwr = preds$Lower,
        upr = preds$Upper
    )

    y_max <- max(c(100, unlist(as.vector(df_drc))), na.rm = T)
    y_min <- min(c(0, unlist(as.vector(df_drc))), na.rm = T)

    xlabs <- if (!is.null(units)) paste0(xlabs, " (", units, ")") else xlabs

    g <- ggplot(
        df_points,
        aes(
            x = concs,
            y = response
        )
    ) +
        geom_point() +
        geom_line(data = df_drc, aes(x = concs, y = preds), color = "red", inherit.aes = F) +
        geom_ribbon(data = df_drc, aes(x = concs, y = preds, ymin = lwr, ymax = upr), alpha = 0.5, fill = "lightblue", inherit.aes = T) +
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(limits = c(y_min, y_max), breaks = seq(from = ceiling(y_min / 25) * 25, to = floor(y_max / 25) * 25, by = 25)) +
        theme_bw() +
        labs(
            x = xlabs,
            y = ylabs
        ) +
        geom_text(data = stat, aes(x = max(processed_plate$concs), y = y, label = label), vjust = 0, hjust = 1, parse = T) +
        ggtitle(title)
    return(g)
}

# process model to get statistics (e.g. AIC, MSE) etc
process_model <- function(model) {
    summary <- summary(model)
    aic <- AIC(model)
    resid <- residuals(model)
    ssr <- sum(resid^2)
    mse <- 1 / length(resid) * ssr
    return(list(
        model = model,
        summary = summary,
        aic = aic,
        resid = resid,
        ssr = ssr,
        mse = mse
    ))
}
