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

# Given an Excel path, search through lines to find '<>' (top corner of plate) -> reads plate from there
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

# removes concentrations from layout
exclude_concentrations <- function(layout, exclude) {
    for (ex in exclude) {
        layout[ex[1], ex[2]] <- paste0(layout[ex[1], ex[2]], "exclude")
    }
    return(layout)
}

# takes layout plate and matches it to main plate
match_layout <- function(layout, main) {
    layout <- layout[rownames(layout) %in% rownames(main), colnames(layout) %in% colnames(main)]
    return(layout)
}

# Normalisation functions ----
# Normalises DRC data for an IC50 curve
# `y = (y_i / y_nc) * 100`
# Args:
# intensity: numeric vector of intensities representing drugged wells
# negative_control: Average of DMSO-treated (NC-1) wells
ic <- function(intensity, negative_control) {
    (intensity / negative_control) * 100
}

# Normalises DRC data for a GI50 curve
# `y = (y_i - y_0) / (y_nc - y_0) * 100`
# Args:
# intensity: numeric vector of intensities representing drugged wells
# negative_control: Average of DMSO-treated (NC-1) wells
# zero_control: Average of untreated (NC-0) wells from GI50 plate
gi <- function(intensity, negative_control, zero_control) {
    ((intensity - zero_control) / (negative_control - zero_control)) * 100
}

# Normalises data to growth rate, as in Hafner et al., 2016
# y = 2^(log2(y_i/y_0)/ log2(y_nc/y_0)) - 1
gr <- function(intensity, negative_control, zero_control) {
    (2^(log2(intensity / zero_control) / log2(negative_control / zero_control)) - 1) * 100
}

# Normalise intensities from viability assay given plate_layout and path
# Args:
# - main_path: Excel spreadsheet for the main treatment plate generated from SparkControl, see format_standard_xl for details
# - gi50_path: if applicable, path to corresponding gi50 plate generated from SparkControl
# - plate_layout: optional, path to Excel spreadsheet with layout and concentrations, see read_plate_layout for details
# - exclude: optional, wells to exclude as list of vector of rows, columns e.g. list(c('A', 1))
# Returns:
# - df: data.frame of concs, normalised treatment intensities
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
        read_plate_layout(path = "plate_layout_gi50.xlsx")
    } else {
        read_plate_layout(path = "plate_layout_ic50.xlsx")
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
        mutate(
            TRT_INTENSITY_IC = ic(TRT_INTENSITY, XCTRL),
            TRT_INTENSITY_GI = gi(TRT_INTENSITY, XCTRL, X0),
            TRT_INTENSITY_GR = gr(TRT_INTENSITY, XCTRL, X0)
        )

    cat("Processed plate:\n")
    print(head(df))

    return(df)
}

# wrapper for process_plates, for processing lists of plates
# Args:
# - path_list: list of lists, containing path information for the plates. In it's most basic form, list(list(main_path = ''), list(main_path = '')))
# - assay_id: optional, appends assay_id to the return processed_plate
# Returns:
# - list containing processed_plate, model
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
    model <- drm(x, fct = LL.3u(upper = 100))

    return(list(
        plate = all_plates,
        model = model
    ))
}

# gets stats from model, i.e. IC50, GI50, etc
get_model_stats <- function(
    model,
    units = "uM",
    stat_type = "GI50") {
    # get gi50 / ic50
    return(stat)
}

# Returns ggplot given a processed plate dataframe, with log conc on x and norm intensity on y
# Args:
# - processed_plates: list of processed_plate(s) from process_plates()
# - units: one of 'uM', 'nM', defaults to 'uM', set to NULL for no units
# - plate_legend_name: controls name of legend for assay_id
plot_drc <- function(processed_plates,
                     xlabs = "Concentration",
                     ylabs = "Normalised Viability (%)",
                     units = "µM",
                     plate_legend_name = "Assay ID",
                     title = NULL,
                     plot_mean = F) {
    # returns simulated preds corresponding to model curve
    get_model_curve_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        newdata_df <- data.frame(
            concs = 10^seq(log10(max(plate$CONCS, na.rm = T)), log10(min(plate$CONCS, na.rm = T)), length.out = 200)
        )
        preds <- as.data.frame(predict(model, newdata = newdata_df, interval = "confidence"))
        preds$concs <- newdata_df$concs
        preds$assay_id <- unique(plate$ASSAY_ID)
        return(preds)
    }
    drcs <- lapply(processed_plates, get_model_curve_)
    drcs <- do.call(rbind, drcs)

    # get raw data points
    get_data_points_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        df <- data.frame(
            concs = model$data$CONCS,
            response = model$data[, 2],
            assay_id = unique(plate$ASSAY_ID)
        )
        return(df)
    }
    df_points <- lapply(processed_plates, get_data_points_)
    df_points <- do.call(rbind, df_points)

    # if plot_mean, convert any replicates by concentration into mean and plot SE bars
    if (plot_mean) {
        df_points <- df_points %>%
            dplyr::filter(!is.na(concs)) %>%
            group_by(concs, assay_id) %>%
            summarise(
                se = sd(response) / sqrt(n()),
                response = mean(response)
            )
    }

    # get GI50/IC50 stats from model
    get_model_stats_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        stat_type <- gsub("TRT_INTENSITY_", "", colnames(model$data[, 2, drop = F]))

        stats <- as.data.frame(drc::ED(model, respLev = c(10, 50, 90), type = "absolute", display = F))
        colnames(stats) <- c(stat_type, "std_error")
        stats$levels <- rownames(stats)
        stats$assay_id <- unique(plate$ASSAY_ID)
        stats$units <- units

        # Use dynamic column name based on stat_type
        stats <- dplyr::relocate(stats, assay_id, levels, all_of(stat_type), std_error, units)

        rownames(stats) <- NULL

        return(stats)
    }
    stats <- lapply(processed_plates, get_model_stats_)
    stats <- do.call(rbind, stats)

    # plot
    y_max <- max(c(df_points$response, df_points$response + df_points$se, 100, drcs$Upper, drcs$Prediction), na.rm = T)
    y_min <- min(c(df_points$response, df_points$response - df_points$se, drcs$Lower, drcs$Prediction), na.rm = T)

    xlabs <- if (!is.null(units)) paste0(xlabs, " (", units, ")") else xlabs

    g <- ggplot(
        df_points,
        aes(
            x = concs,
            y = response
        )
    ) +
        geom_point(aes(shape = assay_id, color = assay_id)) +
        geom_line(data = drcs, aes(x = concs, y = Prediction, color = assay_id), inherit.aes = F) +
        geom_ribbon(data = drcs, aes(x = concs, y = Prediction, fill = assay_id, ymin = Upper, ymax = Lower), alpha = 0.1, inherit.aes = T) +
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(limits = c(y_min, y_max), breaks = seq(from = ceiling(y_min / 25) * 25, to = floor(y_max / 25) * 25, by = 25)) +
        theme_bw() +
        labs(
            x = xlabs,
            y = ylabs,
            color = plate_legend_name,
            fill = plate_legend_name,
            shape = plate_legend_name
        ) +
        ggtitle(title)

    g <- if (plot_mean) g + geom_errorbar(aes(ymin = response - se, ymax = response + se), width = .1, alpha = .5) else g

    return(list(
        plot = g,
        stats = stats
    ))
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

# saves plot and statistics to a folder
# Args:
# - plot_results: results from plot_drc() or list containing $plot and $stats slots
# - save_folder: folder to save results
# - append_file_name: string to append to end of filename before extension
save_results <- function(plot_results, save_folder, append_file_name = NULL, width = 5, height = 5) {
    plot <- plot_results$plot
    stats <- plot_results$stats

    append_file_name <- if (!is.null(append_file_name)) paste0("_", append_file_name) else NULL

    fig_path <- paste0(save_folder, "/drc_plot", append_file_name, ".svg")

    cat("Saving plot to", fig_path, "\n")
    ggsave(
        fig_path,
        plot,
        width = width,
        height = height,
        units = "in",
        dpi = 600
    )

    stat_path <- paste0(save_folder, "/drc_stats", append_file_name, ".csv")
    cat("Saving stats to", stat_path, "\n")
    write.csv(stats, stat_path, row.names = F)
}
