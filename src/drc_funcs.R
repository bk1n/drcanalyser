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

# Normalise intensities from viability assay given plate_layout and path
# Args:
# - main_path: Excel spreadsheet for the main treatment plate generated from SparkControl, see format_standard_xl for details
# - gi50_path: if applicable, path to corresponding gi50 plate generated from SparkControl
# - plate_layout: optional, path to Excel spreadsheet with layout and concentrations, see read_plate_layout for details
# - exclude: optional, wells to exclude as list of vector of rows, columns e.g. list(c('A', 1))
# Returns:
# - df: data.frame of concs, normalised treatment intensities
process_plate <- function(main_path, gi50_path = NULL, plate_layout = NULL, exclude = NULL) {
    r <- if (!is.null(plate_layout)) read_plate_layout(plate_layout) else if (!is.null(gi50_path)) read_plate_layout(path = "plate_layout_gi50.xlsx") else read_plate_layout(path = "plate_layout_ic50.xlsx")

    layout <- if (!is.null(exclude)) exclude_concentrations(r$layout, exclude) else r$layout
    concs <- r$concs
    gi50 <- r$gi50

    incl_gi50 <- if (!is.null(gi50)) TRUE else FALSE
    if (incl_gi50 & is.null(gi50)) stop("gi50_path supplied but gi50 is not included in plate layout!")

    main_intensity <- format_standard_xl(main_path)
    gi50_intensity <- if (incl_gi50) format_standard_xl(gi50_path) else NULL

    cat("Main intensity:\n")
    print(main_intensity)
    cat("\n")
    cat("GI50 intensity:\n")
    print(gi50_intensity)
    cat("\n")

    # separate intensities into list of trt, neg_ctrl, bckgrnd
    intensity_lst <- list()
    conc_lst <- list()
    for (i in 1:nrow(layout)) {
        for (j in 1:ncol(layout)) {
            l <- layout[i, j]
            c <- concs[i, j]
            intens <- main_intensity[i, j]

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
                intens <- gi50_intensity[i, j]

                if (l == "gi50" & !is.na(l)) {
                    intensity_lst[[l]] <- c(intensity_lst[[l]], intens)
                }
            }
        }
    }

    cat("Processed intensities:\n")
    print(intensity_lst)
    cat("\n")

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
    trt_int_mean <- rowMeans(trt_int, na.rm = T)
    trt_int_se <- apply(trt_int, 1, function(row) sd(row, na.rm = T) / sqrt(length(row)))
    trt_int_norm_percmax <- (trt_int_mean / neg_ctrl) * 100

    df <- data.frame(
        concs = unique(unlist(concs_merged)),
        log_concs = log(concs_merged),
        trt_int_mean = trt_int_mean,
        trt_int_se = trt_int_se,
        trt_int_norm_percmax = trt_int_norm_percmax,
        mean_gi50 = mean_gi50
    )

    if (incl_gi50) cat("Returning GI50\n") else cat("Returning IC50\n")

    return(df)
}

# wrapper for process_plates, for processing lists of plates
# Args:
# - path_list: list of lists, containing path information for the plates. In it's most basic form, list(list(main_path = ''), list(main_path = '')))
# - assay_id: optional, appends assay_id to the return processed_plate
# Returns:
# - list containing processed_plate, model
process_plates <- function(path_list, assay_id = NULL) {
    all_plates <- lapply(path_list, function(p) {
        main_path <- if ("main_path" %in% names(p)) p$main_path else stop("Main path must be supplied!")

        plate <- do.call(process_plate, p)
        return(plate)
    })
    all_plates <- do.call(rbind, all_plates)

    if (!is.null(assay_id)) all_plates$assay_id <- assay_id

    model <- drm(trt_int_norm_percmax ~ concs, data = all_plates, fct = LL.4())
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
                     ylabs = if (stat_type == "GI50") "Growth Inhibition (%)" else "Normalised Response (%)",
                     units = "µM",
                     plate_legend_name = "Assay ID",
                     stat_type = "GI50",
                     title = NULL) {
    # returns simulated preds corresponding to model curve
    get_model_curve_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        newdata_df <- data.frame(
            concs = 10^seq(log10(max(plate$concs)), log10(min(plate$concs)), length.out = 200)
        )
        preds <- as.data.frame(predict(model, newdata = newdata_df, interval = "confidence"))
        preds$concs <- newdata_df$concs
        preds$assay_id <- unique(plate$assay_id)
        return(preds)
    }
    drcs <- lapply(processed_plates, get_model_curve_)
    drcs <- do.call(rbind, drcs)

    # get raw data points
    get_data_points_ <- function(processed_plate) {
        plate <- processed_plate$plate
        df <- data.frame(
            concs = plate$concs,
            response = plate$trt_int_norm_percmax,
            assay_id = unique(plate$assay_id)
        )
        return(df)
    }
    df_points <- lapply(processed_plates, get_data_points_)
    df_points <- do.call(rbind, df_points)

    get_model_stats_ <- function(processed_plate) {
        plate <- processed_plate$plate
        model <- processed_plate$model

        stats <- as.data.frame(drc::ED(model, respLev = c(10, 50, 90), display = F))
        colnames(stats) <- c(stat_type, "std_error")
        stats$levels <- rownames(stats)
        stats$assay_id <- unique(plate$assay_id)
        stats$units <- units

        stats <- dplyr::relocate(stats, assay_id, levels, GI50, std_error, units)

        rownames(stats) <- NULL

        return(stats)
    }
    stats <- lapply(processed_plates, get_model_stats_)
    stats <- do.call(rbind, stats)

    # plot
    y_max <- max(c(df_points$response, 100, drcs$Upper, drcs$Prediction), na.rm = T)
    y_min <- min(c(df_points$response, 0, drcs$Lower, drcs$Prediction), na.rm = T)

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
save_results <- function(plot_results, save_folder, append_file_name = NULL) {
    plot <- plot_results$plot
    stats <- plot_results$stats

    append_file_name <- if (!is.null(append_file_name)) paste0("_", append_file_name) else NULL

    fig_path <- paste0(save_folder, "/drc_plot", append_file_name, ".png")

    cat("Saving plot to", fig_path, "\n")
    ggsave(
        fig_path,
        plot,
        width = 6,
        height = 6,
        units = "in",
        dpi = 600
    )

    stat_path <- paste0(save_folder, "/drc_stats", append_file_name, ".csv")
    cat("Saving stats to", stat_path, "\n")
    write.csv(stats, stat_path, row.names = F)
}
